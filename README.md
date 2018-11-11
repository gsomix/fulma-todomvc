# Functional TodoMVC - [Try It!](https://gsomix.github.io/fulma-todomvc/)

The TodoMVC app written in F#, using [Fable](http://fable.io/), [Elmish](https://fable-elmish.github.io/) and [Fulma](https://mangelmaxime.github.io/Fulma/).

## How to use ?

### In development mode

*If you are using Windows replace `./fake.sh` by `fake.cmd`*

1. Run: `./fake.sh build -t Watch`
2. Go to [http://localhost:8080/](http://localhost:8080/)

*On Unix you may need to run `chmod a+x fake.sh`*

In development mode, we activate:

- [Hot Module Replacement](https://fable-elmish.github.io/hmr/), modify your code and see the change on the fly
- [Redux debugger](https://fable-elmish.github.io/debugger/), allow you to debug each message in your application using [Redux dev tool](https://github.com/reduxjs/redux-devtools)

### Build for production

*If you are using Windows replace `./fake.sh` by `fake.cmd`*

1. Run: `./fake.sh build`
2. All the files needed for deployment are under the `output` folder.
