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
#include "ores.qt/PaginationWidget.hpp"

namespace ores::qt {

PaginationWidget::PaginationWidget(QWidget* parent)
    : QWidget(parent),
      info_label_(new QLabel(this)),
      page_size_combo_(new QComboBox(this)),
      load_all_button_(new QPushButton("Load All", this)),
      layout_(new QHBoxLayout(this)) {

    // Configure info label
    info_label_->setText("Showing 0 of 0 records");

    // Configure page size combo box
    page_size_combo_->addItem("25 per page", 25);
    page_size_combo_->addItem("50 per page", 50);
    page_size_combo_->addItem("100 per page", 100);
    page_size_combo_->addItem("200 per page", 200);
    page_size_combo_->addItem("500 per page", 500);
    page_size_combo_->setCurrentIndex(2); // Default to 100

    // Configure load all button
    load_all_button_->setToolTip("Load all remaining records");
    load_all_button_->setEnabled(false);

    // Setup layout
    layout_->addWidget(info_label_);
    layout_->addStretch();
    layout_->addWidget(new QLabel("Page size:", this));
    layout_->addWidget(page_size_combo_);
    layout_->addWidget(load_all_button_);
    layout_->setContentsMargins(5, 5, 5, 5);

    setLayout(layout_);

    // Connect signals
    connect(page_size_combo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &PaginationWidget::on_page_size_changed);
    connect(load_all_button_, &QPushButton::clicked,
            this, &PaginationWidget::on_load_all_clicked);
}

void PaginationWidget::update_state(std::uint32_t loaded_count,
                                     std::uint32_t total_count) {
    loaded_count_ = loaded_count;
    total_count_ = total_count;

    // Update info label
    QString info_text;
    if (total_count == 0) {
        info_text = "No records";
    } else if (loaded_count >= total_count) {
        info_text = QString("Showing all %1 records").arg(total_count);
    } else {
        info_text = QString("Showing %1 of %2 records")
            .arg(loaded_count)
            .arg(total_count);
    }
    info_label_->setText(info_text);

    // Enable/disable load all button
    const bool has_more = loaded_count < total_count && total_count > 0;
    load_all_button_->setEnabled(has_more);
}

std::uint32_t PaginationWidget::page_size() const {
    return page_size_combo_->currentData().toUInt();
}

void PaginationWidget::on_page_size_changed(int index) {
    if (index >= 0) {
        const auto size = page_size_combo_->itemData(index).toUInt();
        emit page_size_changed(size);
    }
}

void PaginationWidget::on_load_all_clicked() {
    emit load_all_requested();
}

}
