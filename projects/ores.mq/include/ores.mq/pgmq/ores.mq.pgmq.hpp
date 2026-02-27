/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MQ_PGMQ_HPP
#define ORES_MQ_PGMQ_HPP

/**
 * @brief Low-level C++23 wrapper around the pgmq PostgreSQL extension.
 *
 * Mirrors the Npgmq (.NET) API closely. Key types:
 *
 * - mq_exception: thrown on all pgmq errors
 * - queue_info:   result of list_queues() — queue metadata
 * - queue_metrics: result of metrics() — queue statistics
 * - message<T>:   a message retrieved from a queue with typed body
 * - client:       stateless service class; every method takes a
 *                 database::context and is safe to call from any thread
 *
 * Template methods (send, read, read_with_poll, pop, set_visibility_timeout)
 * automatically serialize/deserialize bodies via rfl::json. Pass std::string
 * to bypass JSON (raw JSONB text).
 */
namespace ores::mq::pgmq {}

#endif
