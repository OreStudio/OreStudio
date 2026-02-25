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
#include "ores.qt/CatalogDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_CatalogDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

CatalogDetailDialog::CatalogDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CatalogDetailDialog),
      clientManager_(nullptr),
      createMode_(false),
      readOnly_(false) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    connect(ui_->saveButton, &QPushButton::clicked,
            this, &CatalogDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &CatalogDetailDialog::onDeleteClicked);

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &CatalogDetailDialog::onCloseClicked);
}

CatalogDetailDialog::~CatalogDetailDialog() {
    delete ui_;
}

QTabWidget* CatalogDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* CatalogDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* CatalogDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void CatalogDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CatalogDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CatalogDetailDialog::setCatalog(const dq::domain::catalog& catalog) {
    catalog_ = catalog;
    updateUiFromCatalog();
}

void CatalogDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
}

void CatalogDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->ownerEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(false);
}

void CatalogDetailDialog::updateUiFromCatalog() {
    ui_->nameEdit->setText(QString::fromStdString(catalog_.name));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(catalog_.description));
    ui_->ownerEdit->setText(
        catalog_.owner ? QString::fromStdString(*catalog_.owner) : QString());
    populateProvenance(catalog_.version, catalog_.modified_by, catalog_.performed_by,
                       catalog_.recorded_at, "", catalog_.change_commentary);
}

void CatalogDetailDialog::updateCatalogFromUi() {
    catalog_.name = ui_->nameEdit->text().trimmed().toStdString();
    catalog_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();

    QString owner = ui_->ownerEdit->text().trimmed();
    if (owner.isEmpty()) {
        catalog_.owner = std::nullopt;
    } else {
        catalog_.owner = owner.toStdString();
    }

    catalog_.modified_by = username_;
}

void CatalogDetailDialog::onSaveClicked() {
    if (ui_->nameEdit->text().trimmed().isEmpty()) {
        emit errorMessage(tr("Name is required"));
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorMessage(tr("Not connected to server"));
        return;
    }

    updateCatalogFromUi();
    emit statusMessage(tr("Saving catalog..."));

    QPointer<CatalogDetailDialog> self = this;
    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, catalog = catalog_]() -> SaveResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};

        dq::messaging::save_catalog_request request;
        request.catalog = catalog;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_catalog_request,
            0, std::move(payload));

        auto response_result =
            self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result)
            return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result)
            return {false, "Failed to decompress response"};

        auto response =
            dq::messaging::save_catalog_response::deserialize(*payload_result);
        if (!response)
            return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this,
            [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            QString name = QString::fromStdString(self->catalog_.name);
            emit self->catalogSaved(name);
            self->notifySaveSuccess(tr("Catalog '%1' saved").arg(name));
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void CatalogDetailDialog::onDeleteClicked() {
    QString name = ui_->nameEdit->text().trimmed();
    if (name.isEmpty())
        return;

    auto result = QMessageBox::question(
        this, tr("Confirm Delete"),
        tr("Are you sure you want to delete catalog '%1'?").arg(name),
        QMessageBox::Yes | QMessageBox::No);

    if (result != QMessageBox::Yes)
        return;

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorMessage(tr("Not connected to server"));
        return;
    }

    emit statusMessage(tr("Deleting catalog..."));

    QPointer<CatalogDetailDialog> self = this;
    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, catalogName = name.toStdString()]() -> DeleteResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};

        dq::messaging::delete_catalog_request request;
        request.names.push_back(catalogName);
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_catalog_request,
            0, std::move(payload));

        auto response_result =
            self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result)
            return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result)
            return {false, "Failed to decompress response"};

        auto response =
            dq::messaging::delete_catalog_response::deserialize(*payload_result);
        if (!response)
            return {false, "Invalid server response"};

        if (response->results.empty())
            return {false, "No result returned"};

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(this);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, this,
            [self, watcher, name]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->statusMessage(
                tr("Catalog '%1' deleted successfully").arg(name));
            emit self->catalogDeleted(name);
            self->requestClose();
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

}
