# angrylambdas

## Shell

```
nix-shell shell.nix
```

## Build project

```
stack build --system-ghc
```

or for Nix users 

```
stack build --system.ghc --no-nix
```

## Execute AngryLambdas

```
stack --system-ghc exec angrylambdas
```

or for Nix users 

```
stack --no-nix --system-ghc exec angrylambdas
```