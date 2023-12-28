from Emil Valeev:

git clone git@github.com:neurocult/agency.git
cd agency
$ sudo rm -rf /Users/tarvydas/Library/Caches/go-build/*
go build examples/cli/main.go

./main -model gpt-3.5-turbo -maxTokens 1000 -temp=1 -prompt "Translate to Russian" "I love winter"

---

These flags are parameters:

-model gpt-3.5-turbo -maxTokens 1000 -temp=1 -prompt "Translate to Russian"


You can omit all of them if you don't know what they are for
The most useful one is -prompt - it allows to confugre your pipe with some static prefix that can be added as a system message to every later input



---

$ ./main -model gpt-3.5-turbo -maxTokens 1000 -temp=1 -prompt "Translate to Russian" "I love winter"
error, status code: 401, message: You didn't provide an API key. You need to provide your API key in an Authorization header using Bearer auth (i.e. Authorization: Bearer YOUR_KEY), or as the password field (with blank username) if you're accessing the API from your browser and are prompted for a username and password. You can obtain an API key from https://platform.openai.com/account/api-keys.
