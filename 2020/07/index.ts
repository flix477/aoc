// yeaaaaah let's use purescript next time ok?

import * as fs from 'fs';

import * as A from "fp-ts/lib/Array.js"
import * as O from "fp-ts/lib/Option.js"
import * as R from "fp-ts/lib/Record.js"
import { flow, pipe } from "fp-ts/lib/function.js"

type ContainedBag = [string, number]
type ContainedBags = Record<string, number>
type Rule = [string, ContainedBags]
type Rules = Record<string, ContainedBags>

interface Solution {
  bagsContainingOurs: number,
  bagsWithinOurs: number
}

const split = (sep: string) => (x: string) =>  x.split(sep)
const lines = split("\n")
const recordFromArray = <T>(xs: [string, T][]) =>
  A.reduce<[string, T], Record<string, T>>({}, (acc, [k, v]) => R.insertAt(k, v)(acc))(xs)
const recordFilterTrue = flow(Object.entries, A.filter(([_, v]) => v))
const match = (reg: RegExp) => (x: string) => O.fromNullable(x.match(reg))
const readFile = (x: string) => fs.readFileSync(x, { encoding: "utf8" })
const readLines = flow(readFile, lines)

const parseContainerBag = (x: string) => x.substring(0, x.indexOf(" bags"))

const parseContainedBag: (x: string) => ContainedBag = x =>
  [x.substring(2, x.length - 4), parseInt(x.charAt(0), 10)]

const parseContainedBags: (x: string) => ContainedBags = flow(
  match(/\d [\w ]+ bag/g),
  O.getOrElse<string[]>(A.zero),
  A.map(parseContainedBag),
  recordFromArray
)

const parseRule = (line: string): Rule => [
  parseContainerBag(line),
  parseContainedBags(line)
]

const parseRules = flow(
  A.filter<string>(Boolean),
  A.map<string, Rule>(parseRule),
  recordFromArray
)

const containsBag = (bag: string, rules: Rules) => (results: Record<string, boolean>, from: string): [boolean, Record<string, boolean>] =>
  pipe(
    R.lookup(from, results),
    O.map<boolean, [boolean, Record<string, boolean>]>(p => [p, results]),
    O.getOrElse(() => {
      const to = Object.keys(rules[from]!)
      if (to.includes(bag))
        return [true, results]

      return pipe(
        to,
        A.reduce<string, [boolean, Record<string, boolean>]>([false, results], ([p, results], t) => {
          const [newP, newResults] = containsBag(bag, rules)(results, t)
          return [p || newP, R.insertAt(t, newP)(newResults)]
        })
      )
    })
  )

const countContainsBag = (bag: string) => (rules: Rules) =>
  pipe(
    Object.keys(rules),
    A.reduce<string, Record<string, boolean>>({}, (acc, from) => {
      const [p, acb] = containsBag(bag, rules)(acc, from)
      return R.insertAt(from, p)(acb)
    }),
    recordFilterTrue,
    xs => xs.length
  )

const countWithinBagCached = (rules: Rules) => (cache: Record<string, number>) => (bag: string): [Record<string, number>, number] =>
  pipe(
    R.lookup(bag, cache),
    O.map<number, [Record<string, number>, number]>(count => [cache, count]),
    O.getOrElse(() =>
      A.reduce<[string, number], [Record<string, number>, number]>(
        [cache, 1],
        ([cache, count], [b, n]) => {
          const [innerCache, innerCount] = countWithinBagCached(rules)(cache)(b)
          return [R.insertAt(b, innerCount)(innerCache), count + n * innerCount]
        })(Object.entries(rules[bag]!))
    )
  )


const solve: (bag: string) => (xs: string[]) => Solution = bag => flow(
  parseRules,
  rules => {
    return {
      bagsContainingOurs: countContainsBag(bag)(rules),
      bagsWithinOurs: countWithinBagCached(rules)({})(bag)[1] - 1
    }
  }
)

console.log(pipe(
  "input.txt",
  readLines,
  solve("shiny gold")
))
