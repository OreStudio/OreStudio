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
#ifndef ORES_MQ_DOMAIN_QUEUE_MESSAGE_EVENT_JSON_IO_HPP
#define ORES_MQ_DOMAIN_QUEUE_MESSAGE_EVENT_JSON_IO_HPP

#include <iosfwd>
#include "ores.mq/domain/queue_message_event.hpp"

namespace ores::mq::domain {

/**
 * @brief Dumps a queue_message_event to a stream in a JSON-like format.
 *
 * The payload is represented as its byte count rather than raw bytes to
 * keep log output readable.
 */
std::ostream& operator<<(std::ostream& s, const queue_message_event& v);

}

#endif
