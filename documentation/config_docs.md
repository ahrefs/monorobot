# Repository Configuration

A repository configuration file specifies how notifications should be handled for a given repository. It should be at the root of your monorepo in the main branch. The bot will look for a `notabot.json` file by default, but you can change this behavior with the `--config` flag.

When the bot receives its first incoming GitHub notification, it will query the repository URL to retrieve its configuration file. For subsequent notifications, it will use the cached configuration unless an update is detected.

To update the configuration, simply edit the configuration file and push your changes to GitHub. The bot will detect and apply those changes to the configuration, and will be reflected in the next request onwards.

Refer [here](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/webhook-events-and-payloads) for more information on GitHub event payload structure.

## Options

**Example**
```json
{
    "main_branch_name": "develop",
    "prefix_rules": {
        ...
    },
    "label_rules": {
        ...
    },
    "status_rules": {
        ...
    }
}
```

| value | description | optional | default |
|-|-|-|-|
| `main_branch_name` | main branch used for the repo; filtering notifications about merges of main into other branches | Yes | - |
| `label_rules` | label rules config object | No | - |
| `prefix_rules` | prefix rules config object | No | - |
| `status_rules` | status rules config object | No | - |

## Label Options

**Label rules** apply to PR and issue notifications. If a payload matches multiple rules, they are all included.

**Example**
```json
"label_rules": {
    "default_channel": "default",
    "rules": [
        {
            "allow": [
                "backend"
            ],
            "channel": "backend"
        },
        {
            "allow": [
                "a1"
            ],
            "channel": "a1-bot"
        },
        {
            "allow": [
                "a3"
            ],
            "channel": "a3"
        },
        {
            "allow": [],
            "ignore": [
                "backend",
                "a1",
                "a3"
            ],
            "channel": "frontend-bot"
        }
    ]
},
```

| value | description | optional | default |
|-|-|-|-|
| `default_channel` | default channel to notify if no rules match | Yes | don't notify any channel |
| `rules` | list of `label_rule` objects | No | - |

### Label Rule

A **label rule** specifies whether or not a Slack channel should be notified, based on the labels present in the given payload. For each rule, `ignore` is a blacklist of labels that should not notify the rule's channel, and `allow` is a whitelist of labels that should. If a label exists in both lists, the `ignore` list takes precedence. If an empty `ignore` list is provided, nothing is ignored. If an empty `allow` list is provided, everything is allowed. Both are optional; if neither are provided, the rule will always generate a notification for its channel.

| value | description | optional | default |
|-|-|-|-|
| `allow` | if notifications match any label in this list, they should be routed to the channel | Yes | all labels allowed if no list provided |
| `ignore` | if notifications match any label in this list, they shouldn't be routed to the channel (even if they match any allow labels) | Yes | - |
| `channel` | channel to use as webhook if the rule is matched | No | - |

## Prefix Options

**Prefix rules** apply to push, commit comment, and status notifications. If a filename satisfies a prefix rule, the rule's channel will be notified. If a filename matches multiple rules, only the one that is matched by the *longest prefix* is included.

**Example**
```json
"prefix_rules": {
    "default_channel": "default",
    "rules": [
        {
            "allow": [
                "backend/a1"
            ],
            "channel": "a1"
        },
        {
            "allow": [
                "backend/a5",
                "backend/a4"
            ],
            "channel": "backend"
        },
        {
            "channel": "all-push-events"
        }
    ]
},
```

### Prefix Rule

A **prefix rule** specifies whether or not a Slack channel should be notified, based on the filenames present in the commits associated with the given payload. The semantics for the `allow` and `ignore` fields are the same as those for label rules (see above).

| value | description | optional | default |
|-|-|-|-|
| `allow` | if commit files match any prefix in this list, they should be routed to the channel | Yes | all prefixes allowed if no list provided |
| `ignore` | if commit files match any prefix in this list, they shouldn't be routed to the channel (even if they match any allow prefixes) | Yes | - |
| `channel` | channel to use as webhook if the rule is matched | No | - |

## Status Options

Monorobot supports additional behavior for GitHub status notifications, which are typically triggered by CI builds. A payload of this type contains:

- A `context` field, whose value is the name of the CI pipeline the notificiation is about
- A `state` field, whose value is either `success`, `failure`, `pending`, or `error`

The following takes place when a status notification is received.

1. Check whether a status notification should be *allowed* for further processing or *ignored*, according to a list of **status rules**. The bot will check the list *in order*, and use the policy defined by the first rule that the notification satisfies. If no rule matches, the default behavior of the status state is used:
   - `pending`: `ignore`
   - `failure`: `allow`
   - `error`: `allow`
   - `success`: `allow_once`
1. For those payloads allowed by step 1, if it isn't a main branch build notification, route to the default channel to reduce spam in topic channels. Otherwise, check the notification commit's files according to the prefix rules.

Internally, the bot keeps track of the status of the last allowed payload, for a given pipeline and branch. This information is used to evaluate the status rules (see below).

**Example**

```json
"status_rules": {
    "allowed_pipelines": [
        "default",
        "buildkite/pipeline2"
    ],
    "rules": [
        {
            "on": ["failure"],
            "when": {
                "match": {
                    "field": "description",
                    "re": "^\\(Build #[0-9]+ canceled by .+\\|Failed (exit status 255)\\)$"
                }
            },
            "policy": "ignore"
        },
        { "on": ["pending"], "policy": "ignore"},
        { "on": ["failure", "error"], "policy": "allow"},
        { "on": ["success"], "policy": "allow_once"}
    ]
}
```

| value | description | optional | default |
|-|-|-|-|
| `allowed_pipelines` | a list of pipeline names; if specified, payloads whose pipeline name is not in the list will be ignored immediately, without checking the **status rules**; otherwise, all pipelines will be included in the status rule check | Yes | - |
| `rules` | a list of **status rules** to determine whether to *allow* or *ignore* a payload for further processing | No | - |

### Status Rules

A **status rule** specifies whether a GitHub status notification should generate a Slack notification, given the notification's status and the last allowed build status. There are three policy options for handling payloads:

- `allow`: Notify every time.
- `ignore`: Suppress every time.
- `allow_once`: Only notify if the last allowed status notification's build status for the given pipeline and branch differs from the current one. Useful for suppressing consecutive build success notifications.

For example, the rule:
```
{ "on": A, "when": B, "policy": C }
```
is interpreted as:

> "on a notification with a build state in `A`, when condition `B` is met, adopt the policy `C`".

| value | description | optional | default |
|-|-|-|-|
| `on` | a list of build states that can trigger this rule | No | - |
| `when` | a **status condition** object which, if specified, must be true for the rule to match | Yes | - |
| `policy` | a policy option (one of `allow`, `ignore`, `allow_once`) | No | - |

### Status Conditions

You can optionally provide a **status condition** to specify additional requirements that a notification payload should meet, in order for a status rule's policy to apply.

- `all_of`: Matches if every sub-condition in a list is true. Value should be the list of sub-conditions.
- `one_of`: Matches if at least one sub-condition in a list is true. Value should be the list of sub-conditions.
```json
{
    "all_of/one_of": [ condition1, condition2, ...]
}
```
- `not`: Matches if a sub-condition is false. Value should be the sub-condition.
```json
{
    "not": condition
}
```
- `match`: Matches a text field in the payload against a regular expression. Value should be an object of the form:
```json
{
    "match": {
        "field": "context" | "description" | "target_url",
        "re": string // a regular expression
    }
}
```
