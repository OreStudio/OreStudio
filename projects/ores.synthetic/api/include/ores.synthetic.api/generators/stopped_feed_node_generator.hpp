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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_generator.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_API_GENERATORS_STOPPED_FEED_NODE_GENERATOR_HPP
#define ORES_SYNTHETIC_API_GENERATORS_STOPPED_FEED_NODE_GENERATOR_HPP

#include "ores.synthetic.api/domain/stopped_feed_node.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::synthetic::generators {

/**
 * @brief Generates a synthetic stopped_feed_node.
 */
ORES_SYNTHETIC_API_EXPORT domain::stopped_feed_node
generate_synthetic_stopped_feed_node(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic stopped_feed_nodes.
 */
ORES_SYNTHETIC_API_EXPORT std::vector<domain::stopped_feed_node>
generate_synthetic_stopped_feed_nodes(std::size_t n, utility::generation::generation_context& ctx);

}

#endif
