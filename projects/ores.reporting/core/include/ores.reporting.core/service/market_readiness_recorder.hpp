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
#ifndef ORES_REPORTING_CORE_SERVICE_MARKET_READINESS_RECORDER_HPP
#define ORES_REPORTING_CORE_SERVICE_MARKET_READINESS_RECORDER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/market_readiness.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::reporting::service {

/**
 * @brief What a readiness record is about: one run, and the date it read at.
 */
struct readiness_subject {
    boost::uuids::uuid party_id;
    boost::uuids::uuid report_instance_id;
    std::chrono::system_clock::time_point as_of;
};

/**
 * @brief Writes a run's market readiness at the three grains a reader asks it at.
 *
 * The grains are written detail first and summary last, so the counts only exist
 * once the rows they count do: a run that fails part way through a write leaves
 * detail with no summary rather than a summary claiming detail that is not there.
 *
 * @return An empty string on success, or why the record was not written.
 */
class ORES_REPORTING_CORE_EXPORT market_readiness_recorder final {
public:
    explicit market_readiness_recorder(const ores::database::context& ctx);

    [[nodiscard]] std::string record(const readiness_subject& run,
                                     const market_readiness& readiness);

private:
    ores::database::context ctx_;
};

}

#endif
