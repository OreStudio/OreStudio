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
#ifndef ORES_HTTP_ROUTES_REPORTING_RUN_LOOKUP_HPP
#define ORES_HTTP_ROUTES_REPORTING_RUN_LOOKUP_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/domain/report_instance.hpp"
#include <optional>
#include <string>

namespace ores::http::routes::reporting {

/**
 * @brief The run, when the caller's own token can read it, or nothing.
 *
 * Two units of this part answer about one run, so the read that finds it is
 * stated once rather than in each of them.
 */
std::optional<ores::reporting::domain::report_instance>
run_of(const ores::http::domain::http_request& req,
       ores::nats::service::nats_client& session,
       const std::string& id);

}

#endif
