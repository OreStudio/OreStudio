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
#ifndef ORES_SYNTHETIC_API_FEEDS_FEED_OVERVIEW_HPP
#define ORES_SYNTHETIC_API_FEEDS_FEED_OVERVIEW_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/messaging/feed_overview_protocol.hpp"
#include <string>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief The feeds a folder holds itself, not those of the folders under it.
 *
 * A feed sits in one folder, so these are the counts the database groups by
 * folder. The roll-up adds them up the hierarchy.
 */
struct folder_feed_count {
    std::string folder_id;
    int fx_feeds = 0;
    int ir_feeds = 0;
    int running_feeds = 0;
};

/**
 * @brief The children of one folder, each with the feeds under it at any depth.
 *
 * @p folders is every folder the caller sees, with its counts at zero.
 * @p parent_id names the folder whose children are wanted; empty wants the
 * roots. A child's counts are its own and those of every folder below it, and
 * its has_children says whether any folder is below it. The children come back
 * in name order, then by id, so the order is the same on every call.
 */
ORES_SYNTHETIC_API_EXPORT std::vector<messaging::feed_folder>
children_with_counts(const std::vector<messaging::feed_folder>& folders,
                     const std::vector<folder_feed_count>& direct,
                     const std::string& parent_id);

}

#endif
