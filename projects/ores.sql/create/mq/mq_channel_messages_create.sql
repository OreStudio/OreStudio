/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

-- STUB: populated by chat feature. Structure documented here for schema awareness.
-- ores_mq_channel_messages_tbl will store persistent multi-reader messages.
-- Trigger will NOTIFY ores_mq_channel_{queue_id} for real-time delivery.
-- Schema: id bigserial, queue_id uuid, sender_id uuid, sender_type text (human/system/llm),
--   message_type text (CHAT_MSG/SYSTEM_ALERT/RICH_DATA), payload jsonb, raw_payload bytea,
--   created_at timestamptz, PRIMARY KEY (id, created_at).
-- Hypertable + retention policy added when chat feature is implemented.
