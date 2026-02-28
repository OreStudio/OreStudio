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
#include "ores.qt/DataDomainDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_DataDomainDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

DataDomainDetailDialog::DataDomainDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::DataDomainDetailDialog),
      clientManager_(nullptr),
      isCreateMode_(true),
      isReadOnly_(false) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupConnections();
}

DataDomainDetailDialog::~DataDomainDetailDialog() {
    delete ui_;
}

QTabWidget* DataDomainDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* DataDomainDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* DataDomainDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void DataDomainDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked,
            this, &DataDomainDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &DataDomainDetailDialog::onDeleteClicked);
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &DataDomainDetailDialog::onCloseClicked);
}

void DataDomainDetailDialog::setCreateMode(bool create) {
    isCreateMode_ = create;
    setProvenanceEnabled(!create);
    updateUiState();
}

void DataDomainDetailDialog::setDomain(
    const dq::domain::data_domain& domain) {
    domain_ = domain;

    ui_->nameEdit->setText(QString::fromStdString(domain.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(domain.description));

    populateProvenance(domain_.version, domain_.modified_by, domain_.performed_by,
                       domain_.recorded_at, "", domain_.change_commentary);

    updateUiState();
}

void DataDomainDetailDialog::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateUiState();
}

void DataDomainDetailDialog::updateUiState() {
    ui_->nameEdit->setReadOnly(!isCreateMode_ || isReadOnly_);
    ui_->descriptionEdit->setReadOnly(isReadOnly_);

    ui_->saveButton->setVisible(!isReadOnly_);
    ui_->deleteButton->setVisible(!isCreateMode_ && !isReadOnly_);
}

void DataDomainDetailDialog::onSaveClicked() {
    QString name = ui_->nameEdit->text().trimmed();
    QString description = ui_->descriptionEdit->toPlainText().trimmed();

    if (name.isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation Error"),
                                  tr("Name is required."));
        return;
    }

    dq::domain::data_domain domain;
    domain.name = name.toStdString();
    domain.description = description.toStdString();
    domain.modified_by = username_;
    domain.version = isCreateMode_ ? 0 : domain_.version;

    QPointer<DataDomainDetailDialog> self = this;

    struct SaveResult { bool success; std::string message; };

    auto task = [self, domain]() -> SaveResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::save_data_domain_request request;
        request.domains.push_back(domain);
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_data_domain_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::save_data_domain_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this, [self, watcher, name]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->domainSaved(name);
            self->notifySaveSuccess(tr("Data domain '%1' saved").arg(name));
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    emit statusMessage(tr("Saving..."));
    watcher->setFuture(QtConcurrent::run(task));
}

void DataDomainDetailDialog::onDeleteClicked() {
    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"),
        tr("Delete data domain '%1'?").arg(ui_->nameEdit->text()),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<DataDomainDetailDialog> self = this;
    QString name = ui_->nameEdit->text();

    auto task = [self, name]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_data_domain_request request;
        request.names.push_back({name.toStdString()});
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_data_domain_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_data_domain_response::deserialize(*payload_result);
        return response && response->success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher, name]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (success) {
            emit self->statusMessage(tr("Data domain deleted successfully"));
            emit self->domainDeleted(name);
            self->requestClose();
        } else {
            emit self->errorMessage(tr("Failed to delete data domain"));
        }
    });

    emit statusMessage(tr("Deleting..."));
    watcher->setFuture(QtConcurrent::run(task));
}

}
