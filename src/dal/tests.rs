
use time;
use time::Timespec;
use std::thread;
use dal::{get_mysql_pool, get_postgres_pool, get_sqlite_pool};


#[test]
fn mysql_query_pool() {
    const DB_URL : &'static str = "mysql://cirrus:password@localhost:3306/rso_01";
    //const DB_URL : &'static str = "mysql://apps:appspass@localhost:3416/Apps_Admin_DEV";
    //const DB_URL : &'static str = "mysql://apps:appspass@127.0.0.1:3416/Apps_Admin_Prod";
    //const DB_URL : &'static str = "mysql://apps:appspass@172.17.0.2:3416/Apps_Admin_Prod";

    println!("MySQL_DB_URL: {}", DB_URL);
    let pool = get_mysql_pool(DB_URL, &5u32);

    println!("Returning Pool for: {}: {:?}", DB_URL, pool);

    let mut tasks = vec![];

    for _ in 0..3 {
        let pool = pool.clone();
        let th = thread::spawn(move || {
            let mut conn = pool.get().
                map_err(|err| error!("get connection from pool error in line:{} ! error: {:?}", line!(), err) ).unwrap();
            conn.query("select * from member").
                map_err(|err| error!("execute query error in line:{} ! error: {:?}", line!(), err) ).unwrap();
        });
        tasks.push(th);
    }

    for th in tasks {
        let _ = th.join();
    }

    assert_eq!(1, 1);
}

#[derive(Debug)]
struct Person {
    id: i32,
    name: String,
    time_created: Timespec,
    data: Option<Vec<u8>>
}

#[test]
fn sqlite_query_pool() {

    const DB_URL : &'static str = "/tmp/some.db";
    println!("SQLite_DB_URL: {}", DB_URL);

    let pool = get_sqlite_pool(DB_URL, &5u32);
    let conn = pool.get().unwrap();

    conn.execute("DROP TABLE IF EXISTS person", &[]).unwrap();
    conn.execute("CREATE TABLE person (
                  id              INTEGER PRIMARY KEY,
                  name            TEXT NOT NULL,
                  time_created    TEXT NOT NULL,
                  data            BLOB
                  )", &[]).unwrap();
    let me = Person {
        id: 0,
        name: "Steven".to_string(),
        time_created: time::get_time(),
        data: None
    };
    conn.execute("INSERT INTO person (name, time_created, data)
                  VALUES (?1, ?2, ?3)",
                 &[&me.name, &me.time_created, &me.data]).unwrap();

    let mut stmt = conn.prepare("SELECT id, name, time_created, data FROM person").unwrap();
    let person_iter = stmt.query_map(&[], |row| {
        Person {
            id: row.get(0),
            name: row.get(1),
            time_created: row.get(2),
            data: row.get(3)
        }
    }).unwrap();

    for person in person_iter {
        println!("Found person {:?}", person.unwrap());
    }

    assert_eq!(1, 1);
}
