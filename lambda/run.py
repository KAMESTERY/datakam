
from datagql.data import setup
from datagql.schema import schema


def main():
    setup()
    query = '''
        query HeroNameQuery {
          hero {
            name
          }
        }
    '''
    result = schema.execute(query)
    print(result.data)

if __name__ == "__main__":
    # execute only if run as a script
    main()
