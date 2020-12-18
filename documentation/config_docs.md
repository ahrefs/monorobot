# About

Config file is where the variables affecting the behaviour of notabot are defined.

# Configuration values

**example**
```json
{
    "offline": "github-api-cache",
    "main_branch_name": "develop",
    "status_rules": {
        ...
    },
    "prefix_rules": {
        ...
    },
    "label_rules": {
        ...
    },
    "suppress_cancelled_events": true
}
```

| value | description | optional | default |
|-|-|-|-|
| `main_branch_name` | main branch used for the repo; filtering notifications about merges of main into other branches | Yes | - |
| `offline` | path to github api data when http calls are not allowed; used for testing | Yes | - |
| `suppress_cancelled_events` | supresses status cancelled events | Yes | `true` |
| `status_rules` | status rules config object | No | - |
| `label_rules` | label rules config object | No | - |
| `prefix_rules` | prefix rules config object | No | - |

## Status Config

**example**
```json
"status_rules": {
    "allowed_pipelines": [
        "default",
        "buildkite/notabot-test"
    ],
    "status": {
        "pending": false,
        "success": "once",
        "failure": true,
        "error": true,
        "cancelled": "^\\(Build #[0-9]+ canceled by .+\\|Failed (exit status 255)\\)$"
    }
},
```

| value | description | optional | default |
|-|-|-|-|
| `title` | if defines a whitelist of values for the github payload. If not specified, all is permitted. | Yes | - |
| `status` | a `status_state` config object | No | - |

### Status State

A json object with fields of bools for each status type.

| value | description | optional | default |
|-|-|-|-|
| `pending` | `true` to notify; `false` to ignore | No | - |
| `success` | `true` to notify; `false` to notify all; `"once"` to notify the first and ignore subsequent consecutive successes| No | - |
| `failure` | `true` to notify; `false` to ignore | No | - |
| `error` | `true` to notify; `false` to ignore | No | - |
| `cancelled` | provide regex to ignore `failure` notifications with a description that matches it | Yes | - |


## Label Config

Label rules apply to PR and issues notifications.

**example**
```json
"label_rules": {
    "default_channel": "default",
    "rules": [
        {
            "allow": [
                "backend"
            ],
            "ignore": [],
            "channel": "backend"
        },
        {
            "allow": [
                "aa"
            ],
            "ignore": [],
            "channel": "aa-git"
        },
        {
            "allow": [
                "siren"
            ],
            "ignore": [],
            "channel": "siren"
        },
        {
            "allow": [],
            "ignore": [
                "backend",
                "aa",
                "siren"
            ],
            "channel": "frontend-bot"
        }
    ]
},
```

| value | description | optional | default |
|-|-|-|-|
| `default_channel` | default channel to notify if no rules match | Yes | no channels will be notified on default |
| `rules` | list of `label_rule` objects | No | - |

### Label Rule

| value | description | optional | default |
|-|-|-|-|
| `allow` | whitelist of label values that match this rule; if list is empty it vacuously satisfies the rule | No | - |
| `ignore` | blacklist of label values; any labels matching will not match the rule | No | - |
| `channel` | channel to use as webhook if matching this label rule | No | - |

## Prefix Config

Prefix rules apply to filenames. If a filename satisfies a prefix rule, the rule's channel will be notified.

The prefix config object is exactly the same as **Label Config** except its `rules` are list of `prefix_rule` objects.

**example**
```json
"prefix_rules": {
    "default_channel": "default",
    "rules": [
        {
            "allow": [
                "backend/api"
            ],
            "ignore": [],
            "channel": "aa"
        },
        {
            "allow": [
                "backend/megaindex",
                "backend/ahrefskit"
            ],
            "ignore": [],
            "channel": "backend"
        },
        {
            "allow": [],
            "ignore": [],
            "channel": "all-push-events"
        }
    ]
},
```


### Prefix Rule

| value | description | optional | default |
|-|-|-|-|
| `allow` | whitelist of strings that if prefixed in the filename matches the rule | No | - |
| `ignore` | blacklist of strings that if prefixed in the filename does not match the rule | No | - |
| `channel` | channel to use as webhook if matching this prefix rule | No | - |
