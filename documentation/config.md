# About

Config file is where the variables affecting the behaviour of notabot are defined.

# Configuration values

| value | description | optional | default |
|-|-|-|-|
| `main_branch_name` | main branch used for the repo; used or filtering notifications about merges of main into other branches | Yes | - |
| `offline` | path to github api data when http calls are not allowed | Yes | - |
| `suppress_cancelled_events` | supresses status cancelled events | Yes | `true` |
| `status_rules` | status rules config object | No | - |
| `label_rules` | label rules config object | No | - |
| `prefix_rules` | prefix rules config object | No | - |

## Status Config

| value | description | optional | default |
|-|-|-|-|
| `title` | if defines a whitelist of values for the github payload. If not specified, all is permitted. | Yes | - |
| `status` | a `status_state` config object, if true will suppress status notifications of that type | No | - |

### Status State

A json object with fields of bools for each status type. Set them to true to suppress status of that type.

example: 
```json
{
  "success": true,
  "failure": false,
  "pending": false,
  "error": false
}
```

## Label Config

Label rules apply to PR and issues notifications.

| value | description | optional | default |
|-|-|-|-|
| `default` | default channel to notify if no rules match | Yes | no channels will be notified on default |
| `rules` | list of `label_rule` objects | No | - |

### Label Rule

| value | description | optional | default |
|-|-|-|-|
| `label_name` | whitelist of label values that match this rule; if empty it vacuously satisfies the rule | No | - |
| `ignore` | blacklist of label values; any labels matching will not match the rule | No | - |
| `chan` | channel to use as webhook if matching this label rule | No | - |

## Prefix Config

Prefix rules apply to commit and push notifications.

The prefix config object is exactly the same as **Label Config** except its `rules` are list of `prefix_rule` objects.

### Prefix Rule

| value | description | optional | default |
|-|-|-|-|
| `prefix` | whitelist of strings that if exists as prefix to commit or push messages match the rule | No | - |
| `ignore` | blacklist of strings that if exists as prefix to messages will not match the rule | No | - |
| `chan` | channel to use as webhook if matching this prefix rule | No | - |
