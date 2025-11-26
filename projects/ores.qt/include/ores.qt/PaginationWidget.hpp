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
#ifndef ORES_QT_PAGINATION_WIDGET_HPP
#define ORES_QT_PAGINATION_WIDGET_HPP

#include <QWidget>
#include <QLabel>
#include <QPushButton>
#include <QComboBox>
#include <QHBoxLayout>
#include "ores.utility/log/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Widget providing pagination controls for data tables.
 *
 * Displays record count information and navigation buttons for
 * paginated data views. Emits signals when user requests page changes.
 */
class PaginationWidget : public QWidget {
    Q_OBJECT

private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.pagination_widget");
        return instance;
    }

public:
    explicit PaginationWidget(QWidget* parent = nullptr);

    /**
     * @brief Update the pagination display with current state.
     *
     * @param loaded_count Number of records currently loaded
     * @param total_count Total number of records available
     */
    void update_state(std::uint32_t loaded_count, std::uint32_t total_count);

    /**
     * @brief Get the selected page size.
     *
     * @return The number of records per page
     */
    std::uint32_t page_size() const;

    /**
     * @brief Enable or disable the Load All button.
     *
     * @param enabled Whether to enable the button
     */
    void set_load_all_enabled(bool enabled);

signals:
    /**
     * @brief Emitted when user changes the page size selection.
     *
     * @param page_size The new page size
     */
    void page_size_changed(std::uint32_t page_size);

    /**
     * @brief Emitted when user requests to load all remaining data.
     */
    void load_all_requested();

private slots:
    void on_page_size_changed(int index);
    void on_load_all_clicked();

private:
    QLabel* info_label_;
    QComboBox* page_size_combo_;
    QPushButton* first_button_;
    QPushButton* prev_button_;
    QPushButton* next_button_;
    QPushButton* last_button_;
    QPushButton* load_all_button_;
    QHBoxLayout* layout_;

    std::uint32_t loaded_count_{0};
    std::uint32_t total_count_{0};
};

}

#endif
