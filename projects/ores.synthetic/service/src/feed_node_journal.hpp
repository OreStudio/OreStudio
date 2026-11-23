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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_NODE_JOURNAL_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_NODE_JOURNAL_HPP

#include "feed_graph.hpp"
#include "ores.synthetic.api/feeds/node_diagnostics.hpp"
#include <functional>
#include <map>
#include <mutex>
#include <string>
#include <string_view>

namespace ores::synthetic::service {

/**
 * @brief Each feed node's messages: the diagnostic it holds now, and every message it says.
 *
 * A node's messages are telemetry log entries tagged with its name, so a node's page reads its
 * log by its name; the publish is given, so the journal needs no bus. A node holding a
 * diagnostic is failing. See doc/knowledge/architecture/synthetic_feed_graph.org.
 */
class feed_node_journal final : public ores::synthetic::feed::node_diagnostics {
public:
    /** Says one message of one node, at a level: warn, info. */
    using publish_fn = std::function<void(
        const std::string& node, std::string_view level, const std::string& message)>;

    explicit feed_node_journal(publish_fn publish)
        : publish_(std::move(publish)) {}

    void raise(const std::string& node, const std::string& message) override {
        {
            const std::lock_guard lock(mu_);
            auto& held = diagnostics_[node];
            if (held == message)
                return;
            held = message;
        }
        publish_(node, "warn", "Failing: " + message);
    }

    void clear(const std::string& node) override {
        bool held = false;
        {
            const std::lock_guard lock(mu_);
            held = diagnostics_.erase(node) > 0;
        }
        if (held)
            publish_(node, "info", "Produces again.");
    }

    /** The diagnostic each failing node holds now, by its name. */
    [[nodiscard]] std::map<std::string, std::string> diagnostics() const {
        const std::lock_guard lock(mu_);
        return diagnostics_;
    }

    /**
     * Says, to each node whose state differs between two derivations of the graph, its new state
     * and its reason. A node in one graph only is not said.
     */
    void say_changes(const feed_graph& before, const feed_graph& after) {
        std::map<std::string, std::string> was;
        for (const auto& n : before.nodes)
            was.emplace(n.name, n.state);
        for (const auto& n : after.nodes) {
            const auto found = was.find(n.name);
            if (found == was.end() || found->second == n.state)
                continue;
            const auto level = n.state == node_state::running ? "info" : "warn";
            publish_(n.name,
                     level,
                     n.state == node_state::running ? std::string("Running.") :
                                                      "Now " + n.state + ": " + n.reason);
        }
    }

private:
    publish_fn publish_;
    mutable std::mutex mu_;
    std::map<std::string, std::string> diagnostics_;
};

}

#endif
