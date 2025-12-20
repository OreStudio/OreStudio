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
#ifndef ORES_TELEMETRY_DOMAIN_SPAN_LINK_HPP
#define ORES_TELEMETRY_DOMAIN_SPAN_LINK_HPP

#include "ores.telemetry/domain/span_context.hpp"
#include "ores.telemetry/domain/attribute_value.hpp"

namespace ores::telemetry::domain {

/**
 * @brief A link from one span to another, forming hypergraph edges.
 *
 * Links are used to associate spans that are causally related but not in a
 * direct parent-child relationship. This is essential for representing
 * complex relationships such as:
 * - A batch operation triggered by multiple requests
 * - Fan-out/fan-in patterns
 * - Cross-trace correlations (e.g., grid engine work linked to client request)
 *
 * Links enable the construction of a hypergraph where spans can have multiple
 * non-hierarchical relationships.
 */
struct span_link final {
    /**
     * @brief The context of the linked span.
     */
    span_context context;

    /**
     * @brief Additional attributes describing the link relationship.
     *
     * Common attributes might include:
     * - "link.relationship": "triggered_by", "follows_from", "caused_by"
     */
    attributes attrs;
};

}

#endif
