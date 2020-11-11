import os.path
import site

site.addsitedir(os.path.join(os.path.dirname(__file__), 'app'))

if __name__ == "__main__":
    from app.main import run
    run(debug=False)
