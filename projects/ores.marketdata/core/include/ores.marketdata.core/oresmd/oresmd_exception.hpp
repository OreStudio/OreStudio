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
#ifndef ORES_MARKETDATA_CORE_ORESMD_ORESMD_EXCEPTION_HPP
#define ORES_MARKETDATA_CORE_ORESMD_ORESMD_EXCEPTION_HPP

#include "ores.marketdata.core/export.hpp"
#include <boost/exception/info.hpp>
#include <string>

namespace ores::marketdata::core {

/**
 * @brief An oresmd URI failed to parse, or a market_data_requirement could not be resolved
 * into a market_data_identifier.
 *
 * Explicitly exported: thrown from within this shared library and caught by name
 * (catch (const oresmd_exception&)) across the library boundary (e.g. by
 * ores.marketdata.core.tests). Without default visibility, Mach-O's stricter RTTI rules can
 * leave two non-identical typeinfo copies either side of that boundary, so the catch silently
 * never matches even though the thrown message is correct -- reproduced on macOS CI, not on
 * Linux (ELF's weak-symbol merging is more permissive).
 */
class ORES_MARKETDATA_CORE_EXPORT oresmd_exception : public virtual std::exception,
                                                      public virtual boost::exception {
public:
    explicit oresmd_exception(std::string_view message = "")
        : message_(message) {}

    [[nodiscard]] const char* what() const noexcept override {
        return message_.c_str();
    }

private:
    std::string message_;
};

}

#endif
