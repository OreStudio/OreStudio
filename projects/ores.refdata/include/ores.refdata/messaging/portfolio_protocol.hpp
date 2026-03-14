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
#ifndef ORES_REFDATA_MESSAGING_PORTFOLIO_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_PORTFOLIO_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.refdata/domain/portfolio.hpp"

namespace ores::refdata::messaging {

struct get_portfolios_request {
    using response_type = struct get_portfolios_response;
    static constexpr std::string_view nats_subject = "ores.refdata.v1.portfolios.list";
    int offset = 0;
    int limit = 100;
};

struct get_portfolios_response {
    std::vector<ores::refdata::domain::portfolio> portfolios;
    int total_available_count = 0;
};

struct save_portfolio_request {
    using response_type = struct save_portfolio_response;
    static constexpr std::string_view nats_subject = "ores.refdata.v1.portfolios.save";
    ores::refdata::domain::portfolio data;
};

struct save_portfolio_response {
    bool success = false;
    std::string message;
};

struct delete_portfolio_request {
    using response_type = struct delete_portfolio_response;
    static constexpr std::string_view nats_subject = "ores.refdata.v1.portfolios.delete";
    std::vector<std::string> ids;
};

struct delete_portfolio_response {
    bool success = false;
    std::string message;
};

struct get_portfolio_history_request {
    using response_type = struct get_portfolio_history_response;
    static constexpr std::string_view nats_subject = "ores.refdata.v1.portfolios.history";
    std::string id;
};

struct get_portfolio_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::refdata::domain::portfolio> history;
};

}

#endif
