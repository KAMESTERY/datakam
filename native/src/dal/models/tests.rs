
use crate::dal::{
    ThingOutput,
    DocumentInput,
    create_documents,
    update_documents,
    delete_documents
};

#[test]
fn document_crud() {
    let user_id_email = String::from("tttt@tttt.ttt");
    let document_id = String::from("a-test-document");
    
    let doc = DocumentInput {
        name: document_id.clone(),
        user_id: user_id_email.clone(),
        document_id: document_id.clone(),
        data: vec![
            vec![String::from("prop1"), String::from("val1")],
            vec![String::from("prop2"), String::from("val2")]
        ]
    };
    
    println!("--------- Create Documents ----------------");
    
    let doc_id = create_documents(vec![doc]).
        first().unwrap().to_owned();
    
    println!("--------- Get Documents ----------------");
    let thing_output = ThingOutput::get_les_choses(
        user_id_email.clone(),
        vec![document_id.clone()]
    ).unwrap().first().unwrap().to_owned();
    
    println!("--------- Delete Documents ----------------");
    
    delete_documents(user_id_email, vec![doc_id.clone()]);
    
    println!("--------- Documents Assertions ----------------");
    
    assert_eq!(thing_output.thing.name.unwrap(), document_id.clone());
    assert_eq!(doc_id.is_empty(), false);
    assert_eq!(doc_id, document_id);
}
