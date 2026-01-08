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
#include "ores.qt/IconUtils.hpp"

namespace ores::qt {

using namespace ores::telemetry::log;

PaginationWidget::PaginationWidget(QWidget* parent)
    : QWidget(parent),
      info_label_(new QLabel(this)),
      page_size_combo_(new QComboBox(this)),
      first_button_(new QPushButton(this)),
      prev_button_(new QPushButton(this)),
      next_button_(new QPushButton(this)),
      last_button_(new QPushButton(this)),
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

    // Configure navigation buttons with icons
    const QColor iconColor(220, 220, 220);

    first_button_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_previous_20_regular.svg", iconColor));
    first_button_->setToolTip("First page");

    prev_button_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_left_20_regular.svg", iconColor));
    prev_button_->setToolTip("Previous page");

    next_button_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_right_20_regular.svg", iconColor));
    next_button_->setToolTip("Next page");

    last_button_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_next_20_regular.svg", iconColor));
    last_button_->setToolTip("Last page");

    // Initially disabled until we know how many pages there are
    first_button_->setEnabled(false);
    prev_button_->setEnabled(false);
    next_button_->setEnabled(false);
    last_button_->setEnabled(false);

    // Configure load all button
    load_all_button_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_download_20_regular.svg", iconColor));
    load_all_button_->setToolTip("Load all remaining records (max 1000)");
    load_all_button_->setEnabled(false);

    // Setup layout
    layout_->addWidget(info_label_);
    layout_->addStretch();
    layout_->addWidget(first_button_);
    layout_->addWidget(prev_button_);
    layout_->addWidget(next_button_);
    layout_->addWidget(last_button_);
    layout_->addWidget(new QLabel("  ", this)); // Spacer
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

    // Connect navigation buttons
    connect(first_button_, &QPushButton::clicked,
            this, &PaginationWidget::on_first_clicked);
    connect(prev_button_, &QPushButton::clicked,
            this, &PaginationWidget::on_prev_clicked);
    connect(next_button_, &QPushButton::clicked,
            this, &PaginationWidget::on_next_clicked);
    connect(last_button_, &QPushButton::clicked,
            this, &PaginationWidget::on_last_clicked);
}

void PaginationWidget::update_state(std::uint32_t loaded_count,
                                     std::uint32_t total_count) {
    loaded_count_ = loaded_count;
    total_count_ = total_count;

    const auto pages = total_pages();
    const auto current = current_page_;

    // Update info label with page information
    QString info_text;
    if (total_count == 0) {
        info_text = "No records";
    } else if (pages <= 1) {
        info_text = QString("Showing all %1 records").arg(total_count);
    } else {
        info_text = QString("Page %1 of %2 (%3 records)")
            .arg(current + 1)
            .arg(pages)
            .arg(total_count);
    }
    info_label_->setText(info_text);

    update_button_states();

    BOOST_LOG_SEV(lg(), debug) << "update_state: loaded=" << loaded_count
                               << ", total=" << total_count
                               << ", page=" << (current + 1) << "/" << pages;
}

std::uint32_t PaginationWidget::page_size() const {
    return page_size_combo_->currentData().toUInt();
}

void PaginationWidget::on_page_size_changed(int index) {
    if (index >= 0) {
        // Reset to first page when page size changes
        current_page_ = 0;
        const auto size = page_size_combo_->itemData(index).toUInt();
        emit page_size_changed(size);
    }
}

void PaginationWidget::on_load_all_clicked() {
    BOOST_LOG_SEV(lg(), debug) << "Load all button clicked";
    emit load_all_requested();
}

void PaginationWidget::set_load_all_enabled(bool enabled) {
    BOOST_LOG_SEV(lg(), debug) << "set_load_all_enabled(" << enabled
                               << "), button_was_enabled=" << load_all_button_->isEnabled();
    load_all_button_->setEnabled(enabled);
    BOOST_LOG_SEV(lg(), debug) << "After setEnabled(" << enabled
                               << "): button_is_enabled=" << load_all_button_->isEnabled();
}

std::uint32_t PaginationWidget::total_pages() const {
    if (total_count_ == 0) {
        return 0;
    }
    const auto size = page_size();
    return (total_count_ + size - 1) / size;  // Ceiling division
}

std::uint32_t PaginationWidget::current_offset() const {
    return current_page_ * page_size();
}

void PaginationWidget::update_button_states() {
    const auto pages = total_pages();
    const bool has_pages = pages > 1;
    const bool can_go_back = has_pages && current_page_ > 0;
    const bool can_go_forward = has_pages && current_page_ < (pages - 1);

    first_button_->setEnabled(can_go_back);
    prev_button_->setEnabled(can_go_back);
    next_button_->setEnabled(can_go_forward);
    last_button_->setEnabled(can_go_forward);
}

void PaginationWidget::on_first_clicked() {
    if (current_page_ == 0) {
        return;
    }
    BOOST_LOG_SEV(lg(), debug) << "First page clicked";
    current_page_ = 0;
    emit page_requested(current_offset(), page_size());
}

void PaginationWidget::on_prev_clicked() {
    if (current_page_ == 0) {
        return;
    }
    BOOST_LOG_SEV(lg(), debug) << "Previous page clicked, page " << current_page_
                               << " -> " << (current_page_ - 1);
    current_page_--;
    emit page_requested(current_offset(), page_size());
}

void PaginationWidget::on_next_clicked() {
    const auto pages = total_pages();
    if (current_page_ >= pages - 1) {
        return;
    }
    BOOST_LOG_SEV(lg(), debug) << "Next page clicked, page " << current_page_
                               << " -> " << (current_page_ + 1);
    current_page_++;
    emit page_requested(current_offset(), page_size());
}

void PaginationWidget::on_last_clicked() {
    const auto pages = total_pages();
    if (pages == 0 || current_page_ >= pages - 1) {
        return;
    }
    BOOST_LOG_SEV(lg(), debug) << "Last page clicked, going to page " << (pages - 1);
    current_page_ = pages - 1;
    emit page_requested(current_offset(), page_size());
}

}
