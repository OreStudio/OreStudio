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
#ifndef ORES_SYNTHETIC_API_FEEDS_NODE_JOURNAL_HPP
#define ORES_SYNTHETIC_API_FEEDS_NODE_JOURNAL_HPP

#include <string>
#include <string_view>

namespace ores::synthetic::feed {

/**
 * @brief Where a node of the feed graph says what happens to it and why: the one place every
 * part of the synthetic service reports a node's events.
 *
 * Its entries are rare and read by a person on the node's page: a run that starts or stops
 * and why, a state that changes and why, and a diagnostic it raises and clears. The market's
 * numeric record, every tick of the loop, is the scheduler trace, and stays there: what a person
 * reads goes here, what a chart plots goes there. See
 * doc/knowledge/architecture/synthetic_feed_graph.org.
 */
class node_journal {
public:
    virtual ~node_journal() = default;

    /** Says one event of the node, at a level: info or warn. */
    virtual void
    say(const std::string& node, std::string_view level, const std::string& message) = 0;

    /**
     * The node produces nothing, for this reason. Said once when the reason changes; a node
     * holding a diagnostic is failing.
     */
    virtual void raise(const std::string& node, const std::string& message) = 0;

    /** The node produces again. Said only when it held a diagnostic. */
    virtual void clear(const std::string& node) = 0;
};

}

#endif
