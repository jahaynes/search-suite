use std::env;
use std::fmt;
use std::fs;
use std::ops::Deref;
use std::path::{Path, PathBuf};

use uuid::Uuid;

#[cfg(test)]
pub struct TempDirGuard {
    path: PathBuf,
}

#[cfg(test)]
impl Deref for TempDirGuard {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

#[cfg(test)]
impl AsRef<Path> for TempDirGuard {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

#[cfg(test)]
impl AsRef<str> for TempDirGuard {
    fn as_ref(&self) -> &str {
        self.path.to_str().unwrap()
    }
}

#[cfg(test)]
impl fmt::Display for TempDirGuard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.path.to_str().unwrap())
    }
}

#[cfg(test)]
impl Drop for TempDirGuard {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

#[cfg(test)]
pub fn fresh_tmp_dir(prefix: &str) -> TempDirGuard {
    let mut dir = env::temp_dir();
    dir.push(Path::new(&format!("{}{}", prefix, Uuid::new_v4())));
    let path = dir.clone();
    fs::create_dir_all(&path).expect("Failed to create temporary directory");
    TempDirGuard { path }
}
