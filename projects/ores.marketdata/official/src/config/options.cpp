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
#include "ores.marketdata.official/config/options.hpp"
#include <ostream>

namespace ores::marketdata::official::config {

std::ostream& operator<<(std::ostream& s, const options& v) {
    s << "{ frankfurter_url: " << v.frankfurter_url << ", sources: " << v.sources.size()
      << ", backfill_days: " << v.backfill_days << ", poll_minutes: " << v.poll_minutes << " }";
    return s;
}

std::optional<domain::hosted_source> parse_source(const std::string& text) {
    const auto first = text.find(':');
    const auto second = first == std::string::npos ? first : text.find(':', first + 1);
    if (second == std::string::npos || text.find(':', second + 1) != std::string::npos)
        return std::nullopt;
    domain::hosted_source source{
        text.substr(0, first), text.substr(first + 1, second - first - 1), text.substr(second + 1)};
    if (source.source_name.empty() || source.provider_code.empty() ||
        source.frankfurter_key.empty())
        return std::nullopt;
    return source;
}

}
