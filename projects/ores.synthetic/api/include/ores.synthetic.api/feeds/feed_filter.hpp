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
#ifndef ORES_SYNTHETIC_API_FEEDS_FEED_FILTER_HPP
#define ORES_SYNTHETIC_API_FEEDS_FEED_FILTER_HPP

#include "ores.synthetic.api/export.hpp"
#include <string>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief Which feeds a screen means: the filter of the feed table, or the feeds a person picked.
 *
 * The same value drives the page a screen shows and the start or stop it applies, so
 * both name the same feeds. When @p feed_ids is not empty, exactly those feeds are
 * meant: the rest of the filter, @p excluded_ids included, is ignored. Otherwise the
 * feeds are those the filter matches, less @p excluded_ids.
 *
 * An empty text field means no narrowing, and a state of "all" means no narrowing on
 * purpose. The state is decided by the feeds that run when the query is built; a
 * start or stop that runs a moment later counts the feeds that already ran and skips
 * the ones another feed holds, so a stale answer does no harm.
 */
struct feed_filter {
    /** A folder; its feeds and those of the folders under it. Empty for every folder. */
    std::string folder_id;
    /** A provider code. Empty for every provider. */
    std::string provider_code;
    /** An asset class code. Empty for every class. */
    std::string asset_class;
    /** Words that must all appear in the feed's label, source name or URI. */
    std::string search;
    /** all, started or stopped; empty means all. */
    std::string state;
    /** Only the feeds that are enabled, start on their own, and are not running now. */
    bool should_run = false;
    std::vector<std::string> feed_ids;
    std::vector<std::string> excluded_ids;
};

/// The most feed ids, picked or left out, one request may name.
inline constexpr std::size_t most_named_feeds = 2000;

/**
 * @brief Why the filter cannot start or stop feeds, or empty when it can.
 *
 * A start or stop that names no feeds and narrows nothing would run every feed, so it
 * is refused. A state of "all" says that every feed is meant.
 */
ORES_SYNTHETIC_API_EXPORT std::string selection_problem(const feed_filter& filter);

/**
 * @brief Why the filter cannot be used, or empty when it can.
 *
 * Every part that reaches a query is checked here: ids are UUIDs, the state is one of
 * three words, the provider and asset class codes are lower case words.
 */
ORES_SYNTHETIC_API_EXPORT std::string filter_problem(const feed_filter& filter);

/// A text as an escape string literal, so it means the same whatever the server's string settings.
ORES_SYNTHETIC_API_EXPORT std::string sql_literal(const std::string& text);

/**
 * @brief A query that returns every feed of every kind, one row each.
 *
 * The columns are id, kind, label, source_name, oresmd_uri, folder_id, enabled,
 * auto_start, asset_class_code, ticks_per_hour, engine, underlying, and provider_code, with the
 * owning tenant after them. It reads the rows the caller's session sees.
 */
ORES_SYNTHETIC_API_EXPORT std::string feeds_query();

/**
 * @brief The condition that picks the filter's feeds out of feeds_query().
 *
 * @p running names the source names of the feeds that run now; a state of started or
 * stopped is decided by them. @p system_caller says whether the caller works in the system
 * tenant: only it should run the system feeds, so the feeds that should run leave them out
 * for every other caller. The filter must pass filter_problem().
 */
ORES_SYNTHETIC_API_EXPORT std::string where_clause(const feed_filter& filter,
                                                   const std::vector<std::string>& running,
                                                   bool system_caller);

}

#endif
