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
#ifndef ORES_MQ_DOMAIN_QUEUE_DEFINITION_HPP
#define ORES_MQ_DOMAIN_QUEUE_DEFINITION_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.mq/domain/queue_scope_type.hpp"
#include "ores.mq/domain/queue_type.hpp"

namespace ores::mq::domain {

/**
 * @brief Definition of a message queue.
 *
 * Mirrors a row in ores_mq_queues_tbl.
 */
struct queue_definition final {
    boost::uuids::uuid id;
    std::optional<boost::uuids::uuid> tenant_id;
    std::optional<boost::uuids::uuid> party_id;
    queue_scope_type scope_type = queue_scope_type::party;
    queue_type queue_type = queue_type::task;
    std::string name;
    std::string description;
    std::chrono::system_clock::time_point created_at;
    bool is_active = true;
};

}

#endif
