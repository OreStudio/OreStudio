/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <sstream>
#include <string>
#include <vector>
#include <iostream>
#include <iomanip>
#include <rapidcsv.h>
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.risk/csv/currency_mapper.hpp"
#include "ores.risk/csv/exporter.hpp"

namespace ores::risk::csv {

using domain::currency;
using namespace ores::utility::log;

// Properly escape CSV fields according to RFC 4180 with more robust implementation
std::string escape_csv_field(const std::string& field) {
    // Check if field contains special characters that need escaping
    bool needs_quoting = field.empty() || 
                         field.find(',') != std::string::npos || 
                         field.find('"') != std::string::npos || 
                         field.find('\n') != std::string::npos || 
                         field.find('\r') != std::string::npos;
    
    std::string result = field;
    
    // Escape double quotes by doubling them (RFC 4180 section 2)
    size_t pos = 0;
    while ((pos = result.find('"', pos)) != std::string::npos) {
        result.replace(pos, 1, "\"\"");
        pos += 2;  // Move past the replacement
    }
    
    // Add quotes if needed (RFC 4180 section 2)
    if (needs_quoting) {
        result = "\"" + result + "\"";
    }
    
    return result;
}

std::string
exporter::export_currency_config(const std::vector<currency>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Started CSV export. Total: " << v.size();
    BOOST_LOG_SEV(lg(), trace) << "Currencies: " << v;

    std::ostringstream oss;
    
    // Add CSV header following RFC 4180
    oss << "iso_code,name,numeric_code,symbol,fraction_symbol,fractions_per_unit,rounding_type,rounding_precision,format,currency_type,modified_by,valid_from,valid_to\n";
    
    // Add data rows
    for (const auto& curr : v) {
        oss << escape_csv_field(curr.iso_code) << ","
            << escape_csv_field(curr.name) << ","
            << escape_csv_field(curr.numeric_code) << ","
            << escape_csv_field(curr.symbol) << ","
            << escape_csv_field(curr.fraction_symbol) << ","
            << curr.fractions_per_unit << ","
            << escape_csv_field(curr.rounding_type) << ","
            << curr.rounding_precision << ","
            << escape_csv_field(curr.format) << ","
            << escape_csv_field(curr.currency_type) << ","
            << escape_csv_field(curr.modified_by) << ","
            << escape_csv_field(curr.valid_from) << ","
            << escape_csv_field(curr.valid_to) << "\n";
    }

    std::string result = oss.str();
    BOOST_LOG_SEV(lg(), trace) << "CSV: " << result.substr(0, std::min(result.size(), static_cast<size_t>(200))) << "...";

    BOOST_LOG_SEV(lg(), debug) << "Finished CSV export. Result length: " << result.length();
    return result;
}

}