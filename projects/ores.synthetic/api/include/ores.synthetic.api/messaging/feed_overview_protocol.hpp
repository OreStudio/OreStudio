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
 * A collection folder also carries what the collection says: its scope, the provider
 * it stands for, and whether it is bound or sandboxed.
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
    /** @brief For a collection, who it belongs to: system for the installation's, tenant or party;
     * empty otherwise. */
    std::string scope;
    /** @brief For a collection, the provider it stands for; empty for any other folder. */
    std::string provider_code;
    /** @brief For a collection, bound or sandboxed; empty otherwise. */
    std::string binding_mode;
    /** @brief The FX spot feeds under the folder, at any depth. */
    int fx_feeds = 0;
    /** @brief The IR curve feeds under the folder, at any depth. */
    int ir_feeds = 0;
    /** @brief How many of those feeds are running now. */
    int running_feeds = 0;
    /** @brief How many of those feeds are enabled, start on their own, and are not running now. */
    int idle_auto_feeds = 0;
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

/**
 * @brief One feed as the feed tree lists it: enough to name it, to open its
 * page and to tell whether it runs, and nothing of its process.
 */
struct folder_feed {
    /** @brief The feed's id. */
    std::string id;
    /** @brief The feed kind: fx_spot or ir_curve. */
    std::string kind;
    /** @brief What a person calls the feed: the pair, or the currency and the index. */
    std::string label;
    /** @brief The source name the feed publishes under. */
    std::string source_name;
    /** @brief The oresmd URI the feed publishes. */
    std::string oresmd_uri;
    /** @brief The folder the feed sits in; empty when it sits in none. */
    std::string folder_id;
    /** @brief Whether the feed is enabled. */
    bool enabled = false;
    /** @brief The provider the feed's collection stands for. */
    std::string provider_code;
    /** @brief The asset class of the feed: fx or interest_rates. */
    std::string asset_class_code;
    /** @brief What the feed prices: a currency pair such as EUR/USD, or a currency such as USD. */
    std::string underlying;
    /** @brief The process that generates the feed's ticks, as the feed stores it. */
    std::string engine;
    /** @brief How many ticks the feed makes in an hour. */
    int ticks_per_hour = 0;
    /** @brief Whether the feed starts on its own when the service starts. */
    bool auto_start = false;
    /** @brief Whether the feed runs now. */
    bool running = false;
};

/**
 * @brief Request a page of the feeds that sit in one folder, or of the feeds of
 * every folder that match some words.
 */
struct list_folder_feeds_request {
    using response_type = struct list_folder_feeds_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.list_folder_feeds";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The folder whose feeds are wanted, with those of the folders under it; empty for the
     * feeds of every folder. */
    std::string folder_id;
    /** @brief Only the feeds of this provider; empty for every provider. */
    std::string provider_code;
    /** @brief Only the feeds that are enabled, start on their own, and are not running now. */
    bool should_run = false;
    /** @brief Words that must all appear in the feed's label, source name or URI; empty for no
     * search. */
    std::string search;
    /** @brief Which feeds: all, started or stopped; empty means all. */
    std::string state;
    /** @brief Only the feeds of this asset class code; empty for every class. */
    std::string asset_class;
    /** @brief How many feeds to skip. */
    int offset = 0;
    /** @brief How many feeds to return at most. */
    int limit = 60;
};

/**
 * @brief A page of feeds, and how many match in all.
 */
struct list_folder_feeds_response {
    bool success = false;
    std::string message;
    /** @brief How many feeds match, of which the page holds some. */
    int total = 0;
    /** @brief The page of feeds, in label order. */
    std::vector<folder_feed> feeds;
};

/**
 * @brief The feed that holds one series.
 */
struct series_owner {
    /** @brief The series, as the oresmd URI that names it. */
    std::string series;
    /** @brief The source name of the running feed that holds the series; empty when no feed holds
     * it. */
    std::string source_name;
};

/**
 * @brief Ask which running feed holds each of a list of series.
 */
struct resolve_series_owners_request {
    using response_type = struct resolve_series_owners_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.resolve_series_owners";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The series to resolve, each as the oresmd URI that names it. */
    std::vector<std::string> series;
};

/**
 * @brief The owner of each series asked about, in the order asked.
 */
struct resolve_series_owners_response {
    bool success = false;
    std::string message;
    /** @brief The owner of each series asked about, in the order asked. */
    std::vector<series_owner> owners;
};

}

#endif
