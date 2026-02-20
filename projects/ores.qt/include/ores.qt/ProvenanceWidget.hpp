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
#ifndef ORES_QT_PROVENANCE_WIDGET_HPP
#define ORES_QT_PROVENANCE_WIDGET_HPP

#include <chrono>
#include <string>
#include <QWidget>

namespace Ui {
class ProvenanceWidget;
}

namespace ores::qt {

/**
 * @brief Widget displaying the 6 standard record provenance fields.
 *
 * All fields are read-only. Embed one in every detail dialog's Provenance tab
 * via the @c provenanceWidget named widget.
 */
class ProvenanceWidget final : public QWidget {
    Q_OBJECT

public:
    explicit ProvenanceWidget(QWidget* parent = nullptr);
    ~ProvenanceWidget() override;

    /**
     * @brief Populate all provenance fields from entity data.
     *
     * @param recorded_at  Pass a default-constructed time_point to leave
     *                     Recorded At blank (for entities without this field).
     */
    void populate(int version,
                  const std::string& modified_by,
                  const std::string& performed_by,
                  std::chrono::system_clock::time_point recorded_at,
                  const std::string& change_reason_code,
                  const std::string& change_commentary);

    /**
     * @brief Clear all provenance fields.
     */
    void clear();

private:
    Ui::ProvenanceWidget* ui_;
};

}

#endif
