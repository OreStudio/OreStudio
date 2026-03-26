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
#ifndef ORES_QT_WORKUNIT_CREATE_DIALOG_HPP
#define ORES_QT_WORKUNIT_CREATE_DIALOG_HPP

#include <QDialog>
#include <QComboBox>
#include <QLineEdit>
#include <QSpinBox>
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientBatchModel.hpp"
#include "ores.qt/ClientAppVersionModel.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Simple dialog for creating a new compute work unit.
 *
 * Lets the user pick the parent batch, app version, input/config URIs,
 * priority, redundancy target, and audit fields.
 */
class WorkunitCreateDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.workunit_create_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit WorkunitCreateDialog(ClientManager* clientManager,
                                  ChangeReasonCache* changeReasonCache,
                                  QWidget* parent = nullptr);

signals:
    void workunitCreated();

private slots:
    void on_create_clicked();

private:
    void populate_batch_combo();
    void populate_app_version_combo();

    ClientManager*     client_manager_;
    ChangeReasonCache* change_reason_cache_;

    ClientBatchModel*      batch_model_;
    ClientAppVersionModel* app_version_model_;

    QComboBox* batch_combo_;
    QComboBox* app_version_combo_;
    QLineEdit* input_uri_edit_;
    QLineEdit* config_uri_edit_;
    QSpinBox*  priority_spin_;
    QSpinBox*  redundancy_spin_;
    QComboBox* reason_combo_;
    QLineEdit* commentary_edit_;
};

} // namespace ores::qt

#endif
