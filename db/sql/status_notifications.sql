-- @ensure_status_notifications_table
CREATE TABLE IF NOT EXISTS status_notifications (
    id                          INTEGER PRIMARY KEY,
    sha                         VARCHAR(40) NOT NULL,
    notification_text           TEXT NOT NULL,
    commit_author               VARCHAR(255) NOT NULL,
    commit_url                  VARCHAR(255) NOT NULL,
    n_state                     VARCHAR(10) NOT NULL,
    description                 VARCHAR(255) NOT NULL,
    target_url                  VARCHAR(255) NOT NULL,
    build_url                   VARCHAR(255) NOT NULL,
    build_number                INTEGER NOT NULL,
    is_step_notification        BOOLEAN NOT NULL,
    is_canceled                 BOOLEAN NOT NULL,
    context                     VARCHAR(255) NOT NULL,
    repository                  VARCHAR(255) NOT NULL,
    branch                      VARCHAR(255) NOT NULL,
    matched_rule                VARCHAR(255),
    last_handled_in             VARCHAR(255) NOT NULL,
    updated_at                  TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    state_before_notification   TEXT NOT NULL,
    state_after_notification    TEXT NOT NULL,
    has_state_update            BOOLEAN NOT NULL DEFAULT false,
    meta_created_at             TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    meta_updated_at             TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- @insert
INSERT INTO status_notifications VALUES;

-- @update_state
UPDATE status_notifications SET state_before_notification = @before,
 state_after_notification = @after, last_handled_in = @last_handled_in, has_state_update = @has_state_update WHERE id = @id;

-- @update_last_handled_in
UPDATE status_notifications SET last_handled_in = @last_handled_in WHERE id = @id;

-- @update_matched_rule
UPDATE status_notifications SET matched_rule = @rule WHERE id = @id;

-- @get_by_sha
SELECT id, last_handled_in, description, notification_text, n_state, has_state_update, state_before_notification, state_after_notification
FROM status_notifications WHERE sha = ? and context like @pipeline and branch = @branch ORDER BY id DESC;

-- @get_by_build_number
SELECT id, last_handled_in, description, notification_text, n_state, has_state_update, state_before_notification, state_after_notification
FROM status_notifications WHERE build_number = ? and context like @pipeline and branch = @branch ORDER BY id DESC;

-- @get_by_range
SELECT id, last_handled_in, description, notification_text, n_state, has_state_update, state_before_notification, state_after_notification
FROM status_notifications WHERE build_number > ? and context like @pipeline and branch = @branch ORDER BY build_number desc, id desc;

-- @get_by_step_name
SELECT id, last_handled_in, description, notification_text, n_state, has_state_update, state_before_notification, state_after_notification
FROM status_notifications WHERE context LIKE ? and context like @pipeline and branch = @branch ORDER BY build_number desc, id DESC;

