use std::env;
use std::fmt;
use std::fs;
use std::ops::Deref;
use std::panic;
use std::path::{Path, PathBuf};

use uuid::Uuid;

pub fn with_temp_dir<F, T>(prefix: &str, f: F) -> T
where
    F: FnOnce(&Path) -> T,
    F: panic::UnwindSafe,
{
    let random_string = Uuid::new_v4().simple().to_string();
    let temp_dir = env::temp_dir().join(format!("{}{}", prefix, random_string));

    fs::create_dir_all(&temp_dir).expect("Failed to create temporary directory");

    let _guard = TempDirGuard {
        path: temp_dir.clone(),
    };

    f(&temp_dir)
}

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
pub fn fresh_dir() -> TempDirGuard {
    let mut dir = env::temp_dir();
    let uuid = format!("{}", Uuid::new_v4());
    dir.push(Path::new(&uuid));
    let path = dir.clone();
    fs::create_dir_all(&path).expect("Failed to create temporary directory");
    TempDirGuard { path }
}
