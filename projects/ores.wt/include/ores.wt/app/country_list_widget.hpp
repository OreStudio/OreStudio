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
#ifndef ORES_WT_APP_COUNTRY_LIST_WIDGET_HPP
#define ORES_WT_APP_COUNTRY_LIST_WIDGET_HPP

#include <Wt/WContainerWidget.h>
#include <Wt/WTable.h>
#include <Wt/WPushButton.h>
#include <Wt/WText.h>
#include <Wt/WSignal.h>
#include <vector>
#include <string>

namespace ores::wt::app {

/**
 * @brief Country data for display.
 */
struct country_row {
    std::string alpha2_code;
    std::string alpha3_code;
    std::string name;
    std::string numeric_code;
    int version = 0;
};

/**
 * @brief Widget displaying list of countries with CRUD operations.
 */
class country_list_widget : public Wt::WContainerWidget {
public:
    country_list_widget();

    Wt::Signal<>& add_requested() { return add_requested_; }
    Wt::Signal<std::string>& edit_requested() { return edit_requested_; }
    Wt::Signal<std::string>& delete_requested() { return delete_requested_; }

    void refresh();
    void set_countries(const std::vector<country_row>& countries);

private:
    void setup_toolbar();
    void setup_table();
    void populate_table();
    void on_row_clicked(const std::string& alpha2_code);

    Wt::WTable* table_;
    Wt::WText* status_text_;
    Wt::WPushButton* add_button_;
    Wt::WPushButton* refresh_button_;

    std::vector<country_row> countries_;
    Wt::Signal<> add_requested_;
    Wt::Signal<std::string> edit_requested_;
    Wt::Signal<std::string> delete_requested_;
};

}

#endif
