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

#include <string>
#include <chrono>
#include <boost/uuid/uuid.hpp>
#include "ores.mq/domain/queue_scope_type.hpp"
#include "ores.mq/domain/queue_type.hpp"

namespace ores::mq::domain {

struct queue_definition {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    queue_scope_type scope_type = queue_scope_type::tenant;
    queue_type type = queue_type::task;
    std::chrono::system_clock::time_point created_at;
    bool is_active = true;
};

}

#endif
