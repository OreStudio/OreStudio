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
 * Template: cpp_options_header.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_OFFICIAL_CONFIG_OPTIONS_HPP
#define ORES_MARKETDATA_OFFICIAL_CONFIG_OPTIONS_HPP

#include "ores.logging/logging_options.hpp"
#include "ores.marketdata.official/domain/source_feed.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <iosfwd>
#include <optional>
#include <string>
#include <tuple>
#include <vector>

namespace ores::marketdata::official::config {

/**
 * @brief options
 */
struct options final {
    std::optional<ores::logging::logging_options> logging;
    ores::nats::config::nats_options nats;
    /**
     * @brief The address of Frankfurter: the hosted service, or a team's own.
     */
    std::string frankfurter_url;
    /**
     * @brief The address of the New York Fed markets API.
     */
    std::string nyfed_url;
    /**
     * @brief The address of the ECB Data Portal API.
     */
    std::string ecb_url;
    /**
     * @brief The address of the Bank of England's site, which serves its statistical database.
     */
    std::string boe_url;
    /**
     * @brief The address of the Bank of Canada's site, which serves its Valet API.
     */
    std::string boc_url;
    /**
     * @brief The sources the adapter hosts.
     */
    std::vector<domain::hosted_source> sources;
    /**
     * @brief The days a source sends when an id is first demanded.
     */
    std::uint32_t backfill_days{400};
    /**
     * @brief The minutes between two reads of a source once its demand is served.
     */
    std::uint32_t poll_minutes{60};
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<options>) {
    return std::tuple{
        ::ores::utility::codec::field{"logging", &options::logging},
        ::ores::utility::codec::field{"nats", &options::nats},
        ::ores::utility::codec::field{"frankfurter_url", &options::frankfurter_url},
        ::ores::utility::codec::field{"nyfed_url", &options::nyfed_url},
        ::ores::utility::codec::field{"ecb_url", &options::ecb_url},
        ::ores::utility::codec::field{"boe_url", &options::boe_url},
        ::ores::utility::codec::field{"boc_url", &options::boc_url},
        ::ores::utility::codec::field{"sources", &options::sources},
        ::ores::utility::codec::field{"backfill_days", &options::backfill_days},
        ::ores::utility::codec::field{"poll_minutes", &options::poll_minutes},
    };
}

static_assert(::ores::utility::codec::names_every_member<options>(),
              "options has a member its wire shape does not name");

std::ostream& operator<<(std::ostream& s, const options& v);

/**
 * @brief Reads one source as =<source name>:<provider code>:<adapter code>:<key>=, such as
 * =ecb-frankfurter:ecb:frankfurter:ECB=.
 */
std::optional<domain::hosted_source> parse_source(const std::string& text);

}

#endif
