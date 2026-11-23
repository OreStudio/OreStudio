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
#ifndef ORES_SYNTHETIC_API_FEEDS_NODE_DIAGNOSTICS_HPP
#define ORES_SYNTHETIC_API_FEEDS_NODE_DIAGNOSTICS_HPP

#include <string>

namespace ores::synthetic::feed {

/**
 * @brief Where a node of the feed graph says why it cannot produce, and that it can again.
 *
 * A node raises a diagnostic when the reason it produces nothing changes, and clears it when it
 * produces again, so a reader sees each cause once and a node holding a diagnostic is failing.
 * See doc/knowledge/architecture/synthetic_feed_graph.org.
 */
class node_diagnostics {
public:
    virtual ~node_diagnostics() = default;

    /** The node produces nothing, for this reason. */
    virtual void raise(const std::string& node, const std::string& message) = 0;

    /** The node produces again. */
    virtual void clear(const std::string& node) = 0;
};

}

#endif
