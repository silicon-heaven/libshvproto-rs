#![allow(clippy::print_stderr, clippy::print_stdout, clippy::exit, reason = "Fine for a binary")]
use clap::Parser;
use log::LevelFilter;
use shvproto::reader::ContainerType;
use shvproto::reader::ReadErrorReason;
use shvproto::Reader;
use shvproto::Writer;
use shvproto::{ChainPackReader, ChainPackWriter, CponReader, CponWriter};
use simple_logger::SimpleLogger;
use std::fmt::Display;
use std::io::{stdout, BufRead, BufReader, BufWriter};
use std::path::PathBuf;
use std::{fs, io, process};

#[cfg(feature = "cq")]
use jaq_all::{jaq_core::{Ctx, Vars, data::JustLut}, jaq_std};

#[derive(Parser, Debug)]
#[structopt(name = "cp2cp", version = env!("CARGO_PKG_VERSION"), author = env!("CARGO_PKG_AUTHORS"), about = "ChainPack to Cpon and back utility")]
#[expect(clippy::struct_excessive_bools, reason = "Fine for cli args")]
struct Cli {
    #[arg(short, long, help = "Cpon indentation string")]
    indent: Option<String>,
    #[arg(short, long, help = "Do not create oneliners in Cpon indented string output")]
    no_oneliners: bool,
    #[arg(long = "ip", help = "Cpon input")]
    cpon_input: bool,
    #[arg(long = "oc", help = "ChainPack output")]
    chainpack_output: bool,
    /// Expect input data in RPC block transport format, https://silicon-heaven.github.io/shv-doc/rpctransportlayer/stream.html
    #[arg(long)]
    chainpack_rpc_block: bool,
    /// Verbose mode (module, .)
    #[arg(short = 'v', long = "verbose")]
    verbose: Option<String>,
    /// File to process
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,
    /// Run cq with this filter.
    #[cfg(feature = "cq")]
    #[arg(long = "cq")]
    cq_filter: Option<String>,
}

const CODE_SUCCESS: i32 = 0;
const CODE_READ_ERROR: i32 = 1;
const CODE_NOT_ENOUGH_DATA: i32 = 2;
const CODE_WRITE_ERROR: i32 = 4;
#[cfg(feature = "cq")]
const CODE_UNEXPECTED_ERROR: i32 = 5;

#[derive(Debug)]
struct ChainPackRpcBlockResult {
    block_length: Option<usize>,
    frame_length: Option<u64>,
    proto: Option<i64>,
    cpon: String,
}
fn print_option<T: Display>(n: Option<T>) {
    if let Some(n) = n {
        println!("{n}");
    } else {
        println!();
    }
}
fn exit_with_result_and_code(result: &ChainPackRpcBlockResult, error: Option<ReadErrorReason>) -> ! {
    let exit_code = error.map_or(CODE_SUCCESS, |error| match error {
        ReadErrorReason::UnexpectedEndOfStream => CODE_NOT_ENOUGH_DATA,
        ReadErrorReason::NumericValueOverflow => CODE_READ_ERROR,
        ReadErrorReason::InvalidCharacter => {
            eprintln!("Parse input error: {error:?}");
            CODE_READ_ERROR
        }
    });
    print_option(result.block_length);
    print_option(result.frame_length);
    print_option(result.proto);
    println!("{}", result.cpon);
    process::exit(exit_code);
}
fn process_chainpack_rpc_block_and_exit(mut reader: Box<dyn BufRead>) -> ! {
    let mut result = ChainPackRpcBlockResult {
        block_length: None,
        frame_length: None,
        proto: None,
        cpon: "".to_string(),
    };
    let mut rd = ChainPackReader::new(&mut reader);
    #[expect(clippy::cast_possible_truncation, reason = "We assume pointer size is 64-bit")]
    match rd.read_uint_data() {
        Ok(frame_length) => {
            result.block_length = Some(frame_length as usize + rd.position());
            result.frame_length = Some(frame_length);
        }
        Err(e) => {
            exit_with_result_and_code(&result, Some(e.reason));
        }
    };
    match rd.read() {
        Ok(proto) => {
            let proto = proto.as_int();
            if proto == 0 || proto == 1 {
                result.proto = Some(proto);
            } else {
                exit_with_result_and_code(&result, Some(ReadErrorReason::InvalidCharacter));
            }
        }
        Err(e) => {
            exit_with_result_and_code(&result, Some(e.reason));
        }
    }
    match rd.read() {
        Ok(rv) => {
            result.cpon = rv.to_cpon();
            exit_with_result_and_code(&result, None);
        }
        Err(e) => {
            exit_with_result_and_code(&result, Some(e.reason));
        }
    }
}
fn main() {
    // Parse command line arguments
    let mut opts = Cli::parse();

    let mut logger = SimpleLogger::new();
    logger = logger.with_level(LevelFilter::Error);
    if let Some(module_names) = &opts.verbose {
        for module_name in module_names.split(',') {
            let module_name = if module_name == "." {
                module_path!().to_string()
            } else {
                module_name.to_string()
            };
            logger = logger.with_module_level(&module_name, LevelFilter::Trace);
        }
    }
    logger.init().expect("Logger must work");

    if opts.chainpack_rpc_block {
        opts.indent = None;
        opts.chainpack_output = false;
        opts.cpon_input = false;
    }

    let mut reader: Box<dyn BufRead> = match opts.file {
        None => Box::new(BufReader::new(io::stdin())),
        Some(filename) => Box::new(BufReader::new(fs::File::open(filename).expect("Opening files must work"))),
    };

    if opts.chainpack_rpc_block {
        process_chainpack_rpc_block_and_exit(reader)
    }

    let cpon_output = !opts.chainpack_output;
    let use_in_memory_parser = cpon_output && !opts.no_oneliners;
    #[cfg(feature = "cq")]
    let use_in_memory_parser = use_in_memory_parser || opts.cq_filter.is_some();

    if use_in_memory_parser {
        let read_result = if opts.cpon_input {
            let mut rd = CponReader::new(&mut reader);
            rd.read()
        } else {
            let mut rd = ChainPackReader::new(&mut reader);
            rd.read()
        };

        let input_value = match read_result {
            Err(e) => {
                eprintln!("Parse input error: {e:?}");
                process::exit(CODE_READ_ERROR);
            }
            Ok(rv) => rv,
        };

        #[cfg(feature = "cq")]
        let output_values = if let Some(filter) = opts.cq_filter {
            let filter = match jaq_all::compile_with(&filter, jaq_std::defs(), jaq_std::funs(), &[]) {
                Ok(filter) => filter,
                Err(error) => {
                    eprintln!("Failed to parse cq filter: {error:?}");
                    process::exit(CODE_READ_ERROR);
                },
            };

            let ctx = Ctx::<JustLut<shvproto::RpcValue>>::new(&filter.lut, Vars::new([]));
            let outputs = filter.id.run((ctx, input_value));
            outputs.filter_map(|output|
                match output {
                    Ok(rv) => Some(rv),
                    Err(err) => {
                        eprintln!("Unexpected error while processing the cq filter: {err:?}");
                        process::exit(CODE_UNEXPECTED_ERROR)
                    },
                }).collect::<Vec<_>>()
        } else {
            vec![input_value]
        };

        #[cfg(not(feature = "cq"))]
        let output_values = vec![input_value];

        let mut writer = BufWriter::new(stdout());
        for output_value in output_values {
            let res = if opts.chainpack_output {
                let mut wr = ChainPackWriter::new(&mut writer);
                wr.write(&output_value)
            } else {
                let mut wr = CponWriter::new(&mut writer);
                wr.set_no_oneliners(opts.no_oneliners);
                if let Some(s) = &opts.indent {
                    if s == "\\t" {
                        wr.set_indent(b"\t");
                    } else {
                        wr.set_indent(s.as_bytes());
                    }
                }
                wr.write(&output_value)
            };

            if let Err(e) = res {
                eprintln!("Write output error: {e:?}");
                process::exit(CODE_WRITE_ERROR);
            }
        }
    } else {
        let mut rd: Box<dyn Reader + '_> = if opts.cpon_input {
            Box::new(CponReader::new(&mut reader))
        } else {
            Box::new(ChainPackReader::new(&mut reader))
        };

        let mut writer = BufWriter::new(stdout());
        let mut wr: Box<dyn Writer + '_> = if opts.chainpack_output {
            Box::new(ChainPackWriter::new(&mut writer))
        } else {
            let mut wr = Box::new(CponWriter::new(&mut writer));
            if let Some(s) = opts.indent {
                wr.set_indent(s.as_bytes());
            }
            wr
        };
        if let Err(e) = copy_current_value(&mut rd, &mut wr) {
            eprintln!("Copy value error: {e:?}");
            process::exit(CODE_WRITE_ERROR);
        }
    }
}

fn copy_current_value(rd: &mut Box<dyn Reader + '_>, wr: &mut Box<dyn Writer + '_>) -> Result<(), String> {
    use shvproto::reader::ReadSchema;

    let read_token = rd.read_schema().map_err(|e| e.to_string())?;
    // eprintln!("token: {read_token:?}");
    match read_token {
        ReadSchema::ContainerBegin(ContainerType::List) => {
            wr.write_container_begin(ContainerType::List).map_err(|e| e.to_string())?;
            let mut first_item = true;
            loop {
                if rd.is_container_end().map_err(|e| e.to_string())? {
                    let _ = rd.read_schema();
                    wr.write_container_end(ContainerType::List, Some(first_item)).map_err(|e| e.to_string())?;
                    break;
                } else {
                    if !first_item {
                        wr.write_delimiter().map_err(|e| e.to_string())?;
                    }
                    wr.write_indent().map_err(|e| e.to_string())?;
                    copy_current_value(rd, wr)?
                }
                first_item = false;
            }
        }
        ReadSchema::ContainerBegin(container_type) => {
            wr.write_container_begin(container_type).map_err(|e| e.to_string())?;
            let mut first_item = true;
            loop {
                if rd.is_container_end().map_err(|e| e.to_string())? {
                    let _ = rd.read_schema();
                    wr.write_container_end(container_type, Some(first_item)).map_err(|e| e.to_string())?;
                    break;
                } else {
                    if !first_item {
                        wr.write_delimiter().map_err(|e| e.to_string())?;
                    }
                    let key = rd.read_key().map_err(|e| e.to_string())?;
                    wr.write_indent().map_err(|e| e.to_string())?;
                    wr.write_key(&key).map_err(|e| e.to_string())?;
                    copy_current_value(rd, wr)?;
                }
                first_item = false;
            }
            if let ContainerType::MetaMap = container_type {
                return copy_current_value(rd, wr)
            }
        }
        ReadSchema::Scalar => {
            let rv = rd.read().map_err(|e| e.to_string())?;
            wr.write(&rv).map_err(|e| e.to_string())?;
        }
        ReadSchema::ContainerEnd => { }
    }
    Ok(())
}
