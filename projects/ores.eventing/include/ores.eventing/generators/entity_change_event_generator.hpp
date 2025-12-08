/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_EVENTING_GENERATORS_ENTITY_CHANGE_EVENT_GENERATOR_HPP
#define ORES_EVENTING_GENERATORS_ENTITY_CHANGE_EVENT_GENERATOR_HPP

#include "ores.eventing/domain/entity_change_event.hpp"
#include <vector>

namespace ores::eventing::generators {

/**
 * @brief Generates random entity change event instances for testing.
 */
class entity_change_event_generator final {
public:
    /**
     * @brief Generates a single random entity change event.
     */
    static domain::entity_change_event generate();

    /**
     * @brief Generates a vector of random entity change event.
     */
    static std::vector<domain::entity_change_event> generate_set(size_t n);
};

}

#endif
