/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.utility/streaming/std_vector.hpp"
#include "ores.console/importing_configuration.hpp"

namespace ores::console {

importing_configuration::
importing_configuration(importing_configuration&& rhs) noexcept
    : currency_configurations_(std::move(rhs.currency_configurations_)) { }

std::vector<std::filesystem::path>
importing_configuration::currency_configurations() const
{
    return currency_configurations_;
}

void importing_configuration::
currency_configurations(std::vector<std::filesystem::path> v)
{
    currency_configurations_ = std::move(v);
}

bool importing_configuration::
operator==(const importing_configuration& rhs) const
{
    return currency_configurations_ == rhs.currency_configurations();
}

void importing_configuration::swap(importing_configuration& other) noexcept
{
    using std::swap;
    swap(currency_configurations_, other.currency_configurations_);
}

importing_configuration&
importing_configuration::operator=(importing_configuration other)
{
    using std::swap;
    swap(*this, other);
    return *this;
}

std::ostream& operator<<(std::ostream& s, const importing_configuration& v) {
    s << " { "
      << "\"__type__\": "
      << "\"ores::console::importing_configuration\"" << ", "
      << "\"currency_configurations\": " << v.currency_configurations()
      << " }";
    return(s);
}

}
