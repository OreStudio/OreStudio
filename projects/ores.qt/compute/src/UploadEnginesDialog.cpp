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
#include "ores.qt/UploadEnginesDialog.hpp"
#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.compute.api/messaging/app_protocol.hpp"
#include "ores.compute.api/messaging/app_version_platform_protocol.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.api/messaging/platform_protocol.hpp"
#include "ores.compute.client/client/package_publisher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include <QApplication>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QFormLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QProgressBar>
#include <QPushButton>
#include <QVBoxLayout>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

UploadEnginesDialog::UploadEnginesDialog(ClientManager* clientManager,
                                         ChangeReasonCache* changeReasonCache,
                                         const std::string& httpBaseUrl,
                                         QWidget* parent)
    : QDialog(parent)
    , client_manager_(clientManager)
    , change_reason_cache_(changeReasonCache)
    , http_base_url_(httpBaseUrl) {
    setWindowTitle(tr("Upload Engines"));
    setMinimumWidth(480);

    auto* layout = new QVBoxLayout(this);
    auto* form = new QFormLayout;

    app_combo_ = new QComboBox(this);
    form->addRow(tr("Application *"), app_combo_);

    version_combo_ = new QComboBox(this);
    form->addRow(tr("Version *"), version_combo_);

    platform_combo_ = new QComboBox(this);
    form->addRow(tr("Platform *"), platform_combo_);

    auto* file_row = new QHBoxLayout;
    file_edit_ = new QLineEdit(this);
    file_edit_->setReadOnly(true);
    file_edit_->setPlaceholderText(tr("No file selected"));
    browse_btn_ = new QPushButton(tr("Browse…"), this);
    file_row->addWidget(file_edit_);
    file_row->addWidget(browse_btn_);
    form->addRow(tr("Package file *"), file_row);

    reason_combo_ = new QComboBox(this);
    form->addRow(tr("Change reason *"), reason_combo_);

    layout->addLayout(form);

    auto* upload_row = new QHBoxLayout;
    upload_btn_ = new QPushButton(tr("Upload"), this);
    progress_bar_ = new QProgressBar(this);
    progress_bar_->setRange(0, 0);
    progress_bar_->setVisible(false);
    upload_row->addWidget(upload_btn_);
    upload_row->addWidget(progress_bar_, 1);
    layout->addLayout(upload_row);

    status_label_ = new QLabel(this);
    layout->addWidget(status_label_);

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Close, this);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    layout->addWidget(buttons);

    connect(app_combo_,
            QOverload<int>::of(&QComboBox::currentIndexChanged),
            this,
            &UploadEnginesDialog::on_app_selected);
    connect(browse_btn_, &QPushButton::clicked, this, &UploadEnginesDialog::on_browse);
    connect(upload_btn_, &QPushButton::clicked, this, &UploadEnginesDialog::on_upload);

    reload_apps();
    reload_platforms();

    const auto reasons = change_reason_cache_->getReasonsForAmend("common");
    for (const auto& r : reasons)
        reason_combo_->addItem(QString::fromStdString(r.description),
                               QString::fromStdString(r.code));
}

void UploadEnginesDialog::reload_apps() {
    app_combo_->clear();
    apps_.clear();

    compute::messaging::get_apps_request req;
    req.limit = 1000;
    const auto resp = client_manager_->process_authenticated_request(std::move(req));
    if (!resp)
        return;

    apps_ = resp->apps;
    for (const auto& app : apps_)
        app_combo_->addItem(QString::fromStdString(app.name));

    reload_versions();
}

void UploadEnginesDialog::reload_platforms() {
    platform_combo_->clear();
    platforms_.clear();

    compute::messaging::list_platforms_request req;
    const auto resp = client_manager_->process_authenticated_request(std::move(req));
    if (!resp || !resp->success)
        return;

    for (const auto& p : resp->platforms) {
        if (!p.is_active)
            continue;
        platforms_.push_back(p);
        platform_combo_->addItem(QString::fromStdString(p.display_name));
    }
}

void UploadEnginesDialog::on_app_selected(int /*index*/) {
    reload_versions();
}

void UploadEnginesDialog::reload_versions() {
    version_combo_->clear();
    app_versions_.clear();

    const int idx = app_combo_->currentIndex();
    if (idx < 0 || idx >= static_cast<int>(apps_.size()))
        return;
    const auto& app = apps_[idx];

    compute::messaging::get_app_versions_request req;
    req.limit = 1000;
    const auto resp = client_manager_->process_authenticated_request(std::move(req));
    if (!resp)
        return;

    for (const auto& v : resp->app_versions) {
        if (v.app_id != app.id)
            continue;
        app_versions_.push_back(v);
        version_combo_->addItem(QString("%1 / %2").arg(QString::fromStdString(v.wrapper_version),
                                                       QString::fromStdString(v.engine_version)));
    }
}

void UploadEnginesDialog::on_browse() {
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Select Engine Package"), {}, tr("Package Files (*.tar.gz *.zip);;All Files (*)"));
    if (path.isEmpty())
        return;
    file_edit_->setText(path);
}

void UploadEnginesDialog::on_upload() {
    const int app_idx = app_combo_->currentIndex();
    const int version_idx = version_combo_->currentIndex();
    const int platform_idx = platform_combo_->currentIndex();

    if (app_idx < 0 || app_idx >= static_cast<int>(apps_.size()) || version_idx < 0 ||
        version_idx >= static_cast<int>(app_versions_.size()) || platform_idx < 0 ||
        platform_idx >= static_cast<int>(platforms_.size()) || file_edit_->text().isEmpty() ||
        reason_combo_->currentIndex() < 0) {
        MessageBoxHelper::warning(
            this,
            tr("Incomplete"),
            tr("Select an application, version, platform, file, and reason."));
        return;
    }

    const auto& app = apps_[app_idx];
    const auto& version = app_versions_[version_idx];
    const auto& platform = platforms_[platform_idx];
    const auto local_file = file_edit_->text().toStdString();

    upload_btn_->setEnabled(false);
    progress_bar_->setVisible(true);
    status_label_->setText(tr("Uploading…"));
    QApplication::setOverrideCursor(Qt::WaitCursor);

    compute::client::package_publish_result result;
    try {
        compute::client::package_publisher publisher(http_base_url_);
        result = publisher.publish(app.name, version.engine_version, platform.code, local_file);
    } catch (const std::exception& e) {
        QApplication::restoreOverrideCursor();
        upload_btn_->setEnabled(true);
        progress_bar_->setVisible(false);
        status_label_->setText(tr("Upload failed."));
        BOOST_LOG_SEV(lg(), error) << "Upload failed: " << e.what();
        MessageBoxHelper::critical(this, tr("Upload Failed"), QString::fromStdString(e.what()));
        return;
    }

    // save_app_version_request replaces every platform row for the version
    // wholesale, so fetch what's already published and preserve it rather
    // than overwriting other platforms with this single upload. Abort
    // rather than proceed if the fetch itself failed -- treating a fetch
    // failure as "no platforms yet" would silently wipe every previously
    // published platform on save.
    compute::messaging::get_app_version_platforms_by_app_version_request existing_req;
    existing_req.app_version_id = boost::uuids::to_string(version.id);
    const auto existing_resp =
        client_manager_->process_authenticated_request(std::move(existing_req));
    if (!existing_resp || !existing_resp->success) {
        QApplication::restoreOverrideCursor();
        upload_btn_->setEnabled(true);
        progress_bar_->setVisible(false);
        status_label_->setText(tr("Save failed."));
        MessageBoxHelper::critical(
            this,
            tr("Save Failed"),
            tr("Failed to fetch existing platforms; refusing to publish %1 without them "
               "(would wipe other platforms on save).")
                .arg(QString::fromStdString(platform.code)));
        return;
    }

    std::vector<compute::domain::app_version_platform> platform_rows =
        existing_resp->app_version_platforms;
    std::erase_if(platform_rows, [&](const auto& p) { return p.platform_code == platform.code; });

    compute::domain::app_version_platform row;
    row.app_version_id = version.id;
    row.platform_id = platform.id;
    row.platform_code = platform.code;
    row.package_uri = result.package_uri;
    row.sha256 = result.sha256;
    platform_rows.push_back(row);

    // storedUsername() is the raw login string (e.g.
    // "tenant_admin@acme_corporation", used for re-auth); modified_by/
    // performed_by need the resolved bare account username the DB's
    // ores_iam_validate_account_username_fn() actually accepts.
    const auto username = client_manager_->currentUsername();
    const auto reason_code = reason_combo_->currentData().toString().toStdString();

    auto ver = version;
    ver.modified_by = username;
    ver.performed_by = username;
    ver.change_reason_code = reason_code;
    ver.change_commentary = "Published via Upload Engines dialog";

    auto ver_req = compute::messaging::save_app_version_request::from(ver);

    const auto ver_resp = client_manager_->process_authenticated_request(std::move(ver_req));

    QApplication::restoreOverrideCursor();
    upload_btn_->setEnabled(true);
    progress_bar_->setVisible(false);

    if (!ver_resp || !ver_resp->success) {
        const QString msg =
            ver_resp ? QString::fromStdString(ver_resp->message) : tr("No response from server");
        status_label_->setText(tr("Save failed."));
        MessageBoxHelper::critical(this, tr("Save Failed"), msg);
        return;
    }

    // Platforms are saved through the junction's replace-by-app-version flow
    // (compute.v1.app_version_platforms.replace_by_app_version_id), which
    // swaps the full platform row set for the version in one operation.
    compute::messaging::replace_app_version_platforms_by_app_version_request plat_req;
    plat_req.app_version_id = boost::uuids::to_string(ver.id);
    plat_req.app_version_platforms = std::move(platform_rows);
    plat_req.modified_by = username;
    plat_req.performed_by = username;
    plat_req.change_reason_code = reason_code;
    plat_req.change_commentary = ver.change_commentary;

    const auto plat_resp = client_manager_->process_authenticated_request(std::move(plat_req));
    if (!plat_resp || !plat_resp->success) {
        const QString msg =
            plat_resp ? QString::fromStdString(plat_resp->message) : tr("No response from server");
        status_label_->setText(tr("Save failed."));
        MessageBoxHelper::critical(this, tr("Save Failed"), msg);
        return;
    }

    status_label_->setText(tr("Uploaded %1 (sha256=%2).")
                               .arg(QString::fromStdString(result.package_uri),
                                    QString::fromStdString(result.sha256)));
}

}
