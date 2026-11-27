/* -*- mode: c++; tab-width: 4; c-basic-offset: 4 -*-
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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_NODE_HALT_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_NODE_HALT_HPP

#include <mutex>
#include <set>
#include <string>

namespace ores::synthetic::service {

/**
 * @brief The nodes of the feed graph that publish nothing: the ones an operator
 * stopped, and every node those stops starve.
 *
 * The graph is rebuilt whenever a stop or a start lands, and the adapter fleet
 * publishes the resulting set here. A feed reads it on every tick, so a node
 * whose input stopped states nothing from that moment rather than publishing the
 * last value its input left on the board. Shared between the fleet that writes it
 * and the registry that hands it to a feed, because a feed outlives one graph.
 */
class feed_node_halt final {
public:
    void set(std::set<std::string> halted) {
        const std::lock_guard lock(mu_);
        halted_ = std::move(halted);
    }

    [[nodiscard]] bool contains(const std::string& node_name) const {
        const std::lock_guard lock(mu_);
        return halted_.contains(node_name);
    }

private:
    mutable std::mutex mu_;
    std::set<std::string> halted_;
};

}

#endif
