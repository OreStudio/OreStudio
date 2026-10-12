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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_API_MESSAGING_FEED_OVERVIEW_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FEED_OVERVIEW_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief One folder of the feed hierarchy, with what runs under it.
 *
 * A collection folder also carries what the collection says: its scope, the
 * provider it stands for, and whether it is bound or sandboxed.
 */
struct feed_folder {
    /** @brief The folder's id. */
    std::string id;
    /** @brief The parent folder's id; empty for a root. */
    std::string parent_id;
    /** @brief The folder's display name. */
    std::string name;
    /** @brief What the folder is: root, collection, asset_class or instrument_type. */
    std::string kind;
    /** @brief For a collection, who consumes its data: system, tenant or party; empty otherwise. */
    std::string scope;
    /** @brief For a collection, the provider it stands for; empty when none. */
    std::string provider_code;
    /** @brief For a collection, bound or sandboxed; empty otherwise. */
    std::string binding_mode;
    /** @brief The FX spot feeds under the folder, at any depth. */
    int fx_feeds = 0;
    /** @brief The IR curve feeds under the folder, at any depth. */
    int ir_feeds = 0;
    /** @brief How many of those feeds are running now. */
    int running_feeds = 0;
    /** @brief Whether the folder has folders under it. */
    bool has_children = false;
};

/**
 * @brief Request the children of one folder, or the roots, with the feed counts.
 */
struct list_feed_folders_request {
    using response_type = struct list_feed_folders_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.list_feed_folders";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The folder whose children are wanted; empty for the roots. */
    std::string parent_id;
};

/**
 * @brief The children of the folder asked for, and why there are none when
 * the read was refused.
 */
struct list_feed_folders_response {
    bool success = false;
    std::string message;
    /** @brief The children, in name order. */
    std::vector<feed_folder> folders;
};

}

#endif
