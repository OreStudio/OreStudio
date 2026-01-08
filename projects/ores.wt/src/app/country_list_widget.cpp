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
#include "ores.wt/app/country_list_widget.hpp"
#include <Wt/WBreak.h>

namespace ores::wt::app {

country_list_widget::country_list_widget() {
    setStyleClass("country-list-widget");
    setup_toolbar();
    setup_table();
}

void country_list_widget::setup_toolbar() {
    auto toolbar = addWidget(std::make_unique<Wt::WContainerWidget>());
    toolbar->setStyleClass("btn-toolbar mb-3");

    add_button_ = toolbar->addWidget(
        std::make_unique<Wt::WPushButton>("Add Country"));
    add_button_->setStyleClass("btn btn-primary me-2");
    add_button_->clicked().connect([this] {
        add_requested_.emit();
    });

    refresh_button_ = toolbar->addWidget(
        std::make_unique<Wt::WPushButton>("Refresh"));
    refresh_button_->setStyleClass("btn btn-secondary");
    refresh_button_->clicked().connect([this] {
        refresh();
    });

    status_text_ = toolbar->addWidget(std::make_unique<Wt::WText>());
    status_text_->setStyleClass("ms-3 text-muted");
}

void country_list_widget::setup_table() {
    table_ = addWidget(std::make_unique<Wt::WTable>());
    table_->setStyleClass("table table-striped table-hover");
    table_->setHeaderCount(1);

    table_->elementAt(0, 0)->addWidget(
        std::make_unique<Wt::WText>("Alpha-2"));
    table_->elementAt(0, 1)->addWidget(
        std::make_unique<Wt::WText>("Alpha-3"));
    table_->elementAt(0, 2)->addWidget(
        std::make_unique<Wt::WText>("Name"));
    table_->elementAt(0, 3)->addWidget(
        std::make_unique<Wt::WText>("Numeric"));
    table_->elementAt(0, 4)->addWidget(
        std::make_unique<Wt::WText>("Version"));
    table_->elementAt(0, 5)->addWidget(
        std::make_unique<Wt::WText>("Actions"));
}

void country_list_widget::refresh() {
    status_text_->setText("Refreshing...");
}

void country_list_widget::set_countries(
    const std::vector<country_row>& countries) {
    countries_ = countries;
    populate_table();
    status_text_->setText(std::to_string(countries_.size()) + " countries");
}

void country_list_widget::populate_table() {
    while (table_->rowCount() > 1) {
        table_->removeRow(1);
    }

    for (std::size_t i = 0; i < countries_.size(); ++i) {
        const auto& c = countries_[i];
        int row = static_cast<int>(i) + 1;

        table_->elementAt(row, 0)->addWidget(
            std::make_unique<Wt::WText>(c.alpha2_code));
        table_->elementAt(row, 1)->addWidget(
            std::make_unique<Wt::WText>(c.alpha3_code));
        table_->elementAt(row, 2)->addWidget(
            std::make_unique<Wt::WText>(c.name));
        table_->elementAt(row, 3)->addWidget(
            std::make_unique<Wt::WText>(c.numeric_code));
        table_->elementAt(row, 4)->addWidget(
            std::make_unique<Wt::WText>(std::to_string(c.version)));

        auto actions = table_->elementAt(row, 5)->addWidget(
            std::make_unique<Wt::WContainerWidget>());

        auto edit_btn = actions->addWidget(
            std::make_unique<Wt::WPushButton>("Edit"));
        edit_btn->setStyleClass("btn btn-sm btn-outline-primary me-1");
        edit_btn->clicked().connect([this, code = c.alpha2_code] {
            edit_requested_.emit(code);
        });

        auto delete_btn = actions->addWidget(
            std::make_unique<Wt::WPushButton>("Delete"));
        delete_btn->setStyleClass("btn btn-sm btn-outline-danger");
        delete_btn->clicked().connect([this, code = c.alpha2_code] {
            delete_requested_.emit(code);
        });
    }
}

}
