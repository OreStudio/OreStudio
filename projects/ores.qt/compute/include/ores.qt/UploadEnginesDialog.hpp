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
#ifndef ORES_QT_UPLOAD_ENGINES_DIALOG_HPP
#define ORES_QT_UPLOAD_ENGINES_DIALOG_HPP

#include "ores.compute.api/domain/app.hpp"
#include "ores.compute.api/domain/app_version.hpp"
#include "ores.compute.api/domain/compute_platform.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ClientManager.hpp"
#include <QDialog>
#include <string>
#include <vector>

class QComboBox;
class QLabel;
class QLineEdit;
class QPushButton;
class QProgressBar;

namespace ores::qt {

/**
 * @brief Standalone "Upload Engines" dialog: publish a compute engine
 * package (app/version/platform/binary) against an existing app and app
 * version, without going through the full AppProvisionerWizard.
 *
 * Uses the same shared ores.compute.client package_publisher as
 * AppProvisionerWizard's PackageUploadPage and ores.shell's "compute
 * publish-package" command -- the only place this upload logic is
 * implemented. Reachable standalone from the Compute Console for now;
 * designed to slot into the future Acme provisioning wizard's "select
 * ACME" step once that wizard exists.
 */
class UploadEnginesDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.upload_engines_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit UploadEnginesDialog(ClientManager* clientManager,
                                 ChangeReasonCache* changeReasonCache,
                                 const std::string& httpBaseUrl,
                                 QWidget* parent = nullptr);

private slots:
    void on_app_selected(int index);
    void on_browse();
    void on_upload();

private:
    void reload_apps();
    void reload_versions();
    void reload_platforms();

    ClientManager* client_manager_;
    ChangeReasonCache* change_reason_cache_;
    std::string http_base_url_;

    std::vector<compute::domain::app> apps_;
    std::vector<compute::domain::app_version> app_versions_;
    std::vector<compute::domain::compute_platform> platforms_;

    QComboBox* app_combo_;
    QComboBox* version_combo_;
    QComboBox* platform_combo_;
    QComboBox* reason_combo_;
    QLineEdit* file_edit_;
    QPushButton* browse_btn_;
    QPushButton* upload_btn_;
    QProgressBar* progress_bar_;
    QLabel* status_label_;
};

}

#endif
