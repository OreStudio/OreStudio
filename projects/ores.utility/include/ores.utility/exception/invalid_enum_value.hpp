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
#ifndef ORES_UTILITY_EXCEPTION_INVALID_ENUM_VALUE_HPP
#define ORES_UTILITY_EXCEPTION_INVALID_ENUM_VALUE_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <boost/exception/info.hpp>

namespace ores::utility::exception {

/**
 * @brief The exception thrown when an enum has an unexpected value.
 *
 * This exception should be thrown only when:
 *
 * @li the current value of an enumeration is not part of its domain, .e.g. an
 * int was cast into an enumeration with a value which is not valid; or
 *
 * @li a given valid value on an enumeration is not supported in a given
 * context.
 *
 */
class invalid_enum_value : public virtual std::exception,
                           public virtual boost::exception {
  public:
        explicit invalid_enum_value(std::string_view message = "")
        : message_(message) {}

    [[nodiscard]] const char* what() const noexcept override {
        return message_.c_str();
    }

private:
    std::string message_;

};

}

#endif
