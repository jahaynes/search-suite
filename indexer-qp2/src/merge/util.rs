use std::fs;
use std::fs::OpenOptions;
use std::io::Error;
use std::io::ErrorKind::NotFound;

pub fn delete_recreate(dir_name: &str) -> Result<(), Error> {
    delete_dir(dir_name)?;
    fs::create_dir_all(dir_name)
}

fn delete_dir(dir_name: &str) -> Result<(), Error> {
    match fs::remove_dir_all(dir_name) {
      Err(os) =>
        if os.kind() == NotFound {
          Ok(())
        } else {
          Err(os)
        },
      ok => ok
    }
  }

pub fn prepare_empty_file(dest: &str) {
    OpenOptions::new()
                .write(true)
                .create_new(true)
                .truncate(true)
                .open(dest)
                .expect(&format!("Could not prepare_empty_file: {}", dest));
}