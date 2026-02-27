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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MQ_GENERATORS_QUEUE_MESSAGE_EVENT_GENERATOR_HPP
#define ORES_MQ_GENERATORS_QUEUE_MESSAGE_EVENT_GENERATOR_HPP

#include <string>
#include "ores.mq/domain/queue_message_event.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::mq::generators {

/**
 * @brief Generates a synthetic queue_message_event for testing.
 *
 * @param queue_name Queue name for the event. If empty, a name is generated.
 * @param ctx Generation context for reproducible randomness.
 */
domain::queue_message_event generate_synthetic_queue_message_event(
    const std::string& queue_name,
    utility::generation::generation_context& ctx);

/**
 * @brief Generates a synthetic queue_message_event with a JSON payload.
 *
 * The payload is a simple JSON object: {"id":"<uuid>","value":<n>}.
 *
 * @param queue_name Queue name for the event. If empty, a name is generated.
 * @param ctx Generation context for reproducible randomness.
 */
domain::queue_message_event generate_synthetic_json_queue_message_event(
    const std::string& queue_name,
    utility::generation::generation_context& ctx);

}

#endif
