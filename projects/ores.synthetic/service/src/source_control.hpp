/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro
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
#ifndef ORES_SYNTHETIC_SERVICE_SOURCE_CONTROL_HPP
#define ORES_SYNTHETIC_SERVICE_SOURCE_CONTROL_HPP

#include "feed_graph.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief A feed node an operator stops by name that is not a source: an upstream spot or curve,
 * a latent surface, or one vendor's view of a surface.
 */
struct feed_node final {
    std::string name;
    std::string kind;
    std::string provider_code;
    bool stopped = false;
};

/**
 * @brief The nodes of the feed graph an operator stops and starts by name: the simulated sources
 * the service serves, and the upstreams, latent surfaces and vendor views of the system
 * collections.
 *
 * The adapters belong to the service's start up, which builds one for each simulated source of
 * the catalogue; the handler that answers an operator reaches them through this, so it needs
 * neither the catalogue nor the adapters' lifetimes.
 */
class source_control {
public:
    virtual ~source_control() = default;

    /** Every simulated source the service serves, in name order, with its state. */
    [[nodiscard]] virtual std::vector<ores::synthetic::messaging::simulated_source>
    sources() const = 0;

    /** Stops the source's adapter; false when the service serves no source of that name. */
    virtual bool stop(const std::string& source_name) = 0;

    /** Starts the source's adapter again; false when the service serves no source of that name. */
    virtual bool start(const std::string& source_name) = 0;

    /** The node of that name that is not a source, when the graph holds one. */
    [[nodiscard]] virtual std::optional<feed_node> node(const std::string& name) const = 0;

    /** Withholds the node's offers; false when the graph holds no such node. */
    virtual bool stop_node(const std::string& name) = 0;

    /** Offers the node again; false when the graph holds no such node. */
    virtual bool start_node(const std::string& name) = 0;

    /** The feed graph, every node with its state, or the part of it around one node. */
    [[nodiscard]] virtual feed_graph graph(const std::string& around_node) const = 0;
};

}

#endif
