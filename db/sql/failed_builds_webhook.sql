-- @ensure_failed_builds_webhook_table
CREATE TABLE IF NOT EXISTS failed_builds_webhook (
    id                          TEXT PRIMARY KEY,
    sha                         VARCHAR(40) NOT NULL,
    build_payload               TEXT NOT NULL,
    pipeline_payload            TEXT NOT NULL,
    jobs                        TEXT NOT NULL,
    commit_author               VARCHAR(255) NOT NULL,
    commit_url                  VARCHAR(255) NOT NULL,
    build_state                 VARCHAR(10) NOT NULL,
    build_url                   VARCHAR(255) NOT NULL,
    build_number                INTEGER NOT NULL,
    is_canceled                 BOOLEAN NOT NULL,
    pipeline                    VARCHAR(255) NOT NULL,
    repository                  VARCHAR(255) NOT NULL,
    branch                      VARCHAR(255) NOT NULL,
    state_before_notification   TEXT NOT NULL,
    state_after_notification    TEXT NOT NULL,
    last_handled_in             TEXT NOT NULL,
    has_state_update            BOOLEAN NOT NULL,
    notification_created_at     TIMESTAMP NOT NULL,
    created_at                  TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- @create
INSERT INTO failed_builds_webhook VALUES;

-- @update_state_after_notification
UPDATE failed_builds_webhook
SET has_state_update = @has_state_update, state_after_notification = @state_after_notification, last_handled_in = @last_handled_in
WHERE id = @id;

-- @get_by_sha
SELECT id, sha, build_state, build_number, is_canceled, has_state_update, last_handled_in, state_before_notification, state_after_notification
FROM failed_builds_webhook WHERE sha = ? and pipeline = @pipeline and branch = @branch ORDER BY id DESC;

-- @get_by_build_number
SELECT id, sha, build_state, build_number, is_canceled, has_state_update, last_handled_in, state_before_notification, state_after_notification
FROM failed_builds_webhook WHERE build_number = ? and pipeline = @pipeline and branch = @branch ORDER BY id DESC;

-- @get_after
SELECT id, sha, build_state, build_number, is_canceled, has_state_update, last_handled_in, state_before_notification, state_after_notification
FROM failed_builds_webhook WHERE build_number >= ? and pipeline = @pipeline and branch = @branch ORDER BY build_number desc, id desc;

-- @get_from_to
SELECT id, sha, build_state, build_number, is_canceled, has_state_update, last_handled_in, state_before_notification, state_after_notification
FROM failed_builds_webhook WHERE build_number >= ? and build_number <= @to_ and pipeline = @pipeline and branch = @branch ORDER BY build_number desc, id desc;
