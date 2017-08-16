
use std::sync::Arc;
use r2d2;
use r2d2::Pool;
use r2d2_mysql::{MysqlConnectionManager, CreateManager};
use r2d2_postgres::{TlsMode, PostgresConnectionManager};
use r2d2_sqlite::{SqliteConnectionManager};

pub fn get_mysql_pool(db_url: &'static str, size: &u32)
                      -> Arc<Pool<MysqlConnectionManager>> {
    info!("Connecting to: {}", db_url);
    let config = r2d2::config::Builder::new().pool_size(size.clone()).build();
    info!("Creating Pool for: {}", db_url);
    let manager: MysqlConnectionManager = MysqlConnectionManager::new(db_url).unwrap();
    info!("Manager: {:?}", manager);
    let pool = Arc::new(r2d2::Pool::new(config, manager).unwrap());
    info!("Returning Pool for: {}", db_url);
    return pool;
}

pub fn get_postgres_pool(db_url: &'static str, size: &u32)
                      -> Arc<Pool<PostgresConnectionManager>> {
    info!("Connecting to: {}", db_url);
    let config = r2d2::config::Builder::new().pool_size(size.clone()).build();
    info!("Creating Pool for: {}", db_url);
    let manager: PostgresConnectionManager = PostgresConnectionManager::new(
        db_url,
        TlsMode::None).unwrap();
    info!("Manager: {:?}", manager);
    let pool = Arc::new(r2d2::Pool::new(config, manager).unwrap());
    info!("Returning Pool for: {}", db_url);
    return pool;
}

pub fn get_sqlite_pool(db_url: &'static str, size: &u32)
                      -> Arc<Pool<SqliteConnectionManager>> {
    info!("Connecting to: {}", db_url);
    let config = r2d2::config::Builder::new().pool_size(size.clone()).build();
    info!("Creating Pool for: {}", db_url);
    let manager: SqliteConnectionManager = SqliteConnectionManager::new(db_url);
//    info!("Manager: {:?}", manager);
    let pool = Arc::new(r2d2::Pool::new(config, manager).unwrap());
    info!("Returning Pool for: {}", db_url);
    return pool;
}

pub fn print_hello() {
    println!("Hello, world!");
}

#[cfg(test)] mod tests;
