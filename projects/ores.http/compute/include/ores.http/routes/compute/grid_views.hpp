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
#ifndef ORES_HTTP_ROUTES_COMPUTE_GRID_VIEWS_HPP
#define ORES_HTTP_ROUTES_COMPUTE_GRID_VIEWS_HPP

#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.api/domain/result.hpp"
#include "ores.compute.api/domain/workunit.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::compute::grid {

/// The most rows a grid page answers with.
inline constexpr std::uint32_t page_max = 200;

/// The page a grid list answers with when the caller states none.
inline constexpr std::uint32_t page_default = 25;

/**
 * @brief One page of a grid list: how many rows to skip and how many to answer with.
 *
 * The wire page allows 1000 rows. The grid screens hold a page to 200, as the
 * BFF held it, so a screen reads the same page from either server.
 */
struct page final {
    std::uint32_t offset = 0;
    std::uint32_t limit = page_default;
};

/// The sentence a grid page states when it is not one.
inline constexpr std::string_view page_refused =
    "The jobs read takes an offset of 0 or more and a limit of 1 to 200.";

/**
 * @brief The page a grid list's query states.
 *
 * A stated offset or limit that is not a whole number, or a limit outside 1 to
 * 200, is refused with @p sentence. An absent one takes the page's own default.
 */
page page_of(const ores::http::domain::http_request& req, std::string_view sentence);

/**
 * @brief Whether the caller may read the node a run ran on.
 *
 * The node belongs to the installation, so the operator alone reads it until the
 * owner decides who else may.
 */
bool may_read_host(const ores::http::domain::http_request& req);

/**
 * @brief The name of each party the caller's own account works in, by its id.
 *
 * A read that fails leaves every name out and does not fail the page: a party's
 * name is a decoration of the row.
 */
std::map<std::string, std::string> party_names(const ores::http::domain::http_request& req,
                                               ores::nats::service::nats_client& session);

/**
 * @brief The name of each report run that references a run id the caller may read.
 *
 * A reference that is text rather than an id, or an id no run carries, is left
 * out. A read that fails leaves every name out and does not fail the page.
 */
std::map<std::string, std::string> report_run_names(const ores::http::domain::http_request& req,
                                                    ores::nats::service::nats_client& session,
                                                    const std::vector<std::string>& references);

/// The text of an identifier, or nothing when it is unset or the nil uuid.
std::optional<std::string> stated_id(const boost::uuids::uuid& id);

/// The text of an identifier that may be absent.
std::optional<std::string> stated_id(const std::optional<boost::uuids::uuid>& id);

/// One batch as a grid screen reads it, with the run and party that made it.
std::string batch_json(const ores::compute::domain::batch& batch,
                       const std::map<std::string, std::string>& runs,
                       const std::map<std::string, std::string>& parties);

/// One work unit as a grid screen reads it.
std::string workunit_json(const ores::compute::domain::workunit& workunit);

/**
 * @brief One run as a grid screen reads it.
 *
 * The node is stated only where the reader may see it, and @c host_hidden says
 * the reader may not. The instants the service states as the epoch and the
 * duration it leaves unset are stated as null, so a screen never reads an
 * unknown as a real value.
 */
std::string result_json(const ores::compute::domain::result& result, bool host_visible);

/// A page's answer: when it was read, its rows, the count it matches and its page.
std::string page_json(const std::vector<std::string>& rows, std::uint64_t total, const page& asked);

}

#endif
