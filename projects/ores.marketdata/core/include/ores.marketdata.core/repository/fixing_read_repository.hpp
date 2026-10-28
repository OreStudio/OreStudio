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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_FIXING_READ_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_FIXING_READ_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief A series a fixing item names, with the party that owns it and its provider.
 */
struct fixing_series final {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string provider_code;
};

/**
 * @brief One stored fixing, with the time the service stored it.
 */
struct stored_fixing final {
    std::chrono::year_month_day date;
    std::string value;
    std::string source;
    std::chrono::system_clock::time_point received_at;
};

/**
 * @brief The provider's own id of an item.
 */
struct vendor_key final {
    std::string scheme;
    std::string id;
};

/**
 * @brief The reads of the durable fixings by item and date.
 *
 * The generated fixing type does not carry the time a row was stored, which a reader of
 * fixings reports, so these reads are written here. They read current rows only: a fixing
 * corrected later is read as corrected.
 */
class ORES_MARKETDATA_CORE_EXPORT fixing_read_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.fixing_read_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief The series of @p item among the parties the session sees, of @p provider_code
     * only when it is not empty.
     */
    std::vector<fixing_series>
    series_of(context ctx, const std::string& item, const std::string& provider_code);

    /**
     * @brief The latest fixings of @p series_id on or before @p date, newest first.
     */
    std::vector<stored_fixing> on_or_before(context ctx,
                                            const boost::uuids::uuid& series_id,
                                            std::chrono::year_month_day date,
                                            std::size_t limit);

    /**
     * @brief The fixings of @p series_id from @p from to @p to, both included, oldest first.
     */
    std::vector<stored_fixing> between(context ctx,
                                       const boost::uuids::uuid& series_id,
                                       std::chrono::year_month_day from,
                                       std::chrono::year_month_day to,
                                       std::size_t limit);

    /**
     * @brief The vendor id that @p provider_code maps to @p item, if a mapping names it.
     */
    std::optional<vendor_key>
    vendor_of(context ctx, const std::string& item, const std::string& provider_code);
};

}

#endif
