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
#include "ores.refdata.core/repository/currency_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/currency_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::currency currency_mapper::map(const currency_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::currency r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.iso_code = v.iso_code.value();

    r.name = v.name;

    r.numeric_code = v.numeric_code;
    r.symbol = v.symbol;
    r.fraction_symbol = v.fraction_symbol;
    r.fractions_per_unit = v.fractions_per_unit;
    r.rounding_type = v.rounding_type;
    r.rounding_precision = v.rounding_precision;
    r.format = v.format;
    r.monetary_nature = v.monetary_nature;
    r.market_tier = v.market_tier;
    r.image_id = v.image_id.has_value() ?
                     std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.image_id)) :
                     std::nullopt;
    r.spot_days = v.spot_days;
    r.day_basis = v.day_basis;
    r.base_precedence = v.base_precedence;
    r.holiday_calendar = v.holiday_calendar;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

currency_entity currency_mapper::map(const domain::currency& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    currency_entity r;
    r.iso_code = v.iso_code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.name = v.name;

    r.numeric_code = v.numeric_code;
    r.symbol = v.symbol;
    r.fraction_symbol = v.fraction_symbol;
    r.fractions_per_unit = v.fractions_per_unit;
    r.rounding_type = v.rounding_type;
    r.rounding_precision = v.rounding_precision;
    r.format = v.format;
    r.monetary_nature = v.monetary_nature;
    r.market_tier = v.market_tier;
    r.image_id =
        v.image_id.has_value() ? std::optional(boost::uuids::to_string(*v.image_id)) : std::nullopt;
    r.spot_days = v.spot_days;
    r.day_basis = v.day_basis;
    r.base_precedence = v.base_precedence;
    r.holiday_calendar = v.holiday_calendar;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::currency> currency_mapper::map(const std::vector<currency_entity>& v) {
    return map_vector<currency_entity, domain::currency>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<currency_entity> currency_mapper::map(const std::vector<domain::currency>& v) {
    return map_vector<domain::currency, currency_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
