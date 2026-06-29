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
#include "ores.qt/GmmComponentDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ui_GmmComponentDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

GmmComponentDetailDialog::GmmComponentDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::GmmComponentDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

GmmComponentDetailDialog::~GmmComponentDetailDialog() {
    delete ui_;
}

QTabWidget* GmmComponentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* GmmComponentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* GmmComponentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void GmmComponentDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void GmmComponentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &GmmComponentDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &GmmComponentDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &GmmComponentDetailDialog::onCloseClicked);

    connect(ui_->descriptionEdit,
            &QLineEdit::textChanged,
            this,
            &GmmComponentDetailDialog::onFieldChanged);
    connect(
        ui_->meanEdit, &QLineEdit::textChanged, this, &GmmComponentDetailDialog::onFieldChanged);
    connect(
        ui_->stdevEdit, &QLineEdit::textChanged, this, &GmmComponentDetailDialog::onFieldChanged);
    connect(
        ui_->weightEdit, &QLineEdit::textChanged, this, &GmmComponentDetailDialog::onFieldChanged);
}

void GmmComponentDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void GmmComponentDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void GmmComponentDetailDialog::setComponent(const synthetic::domain::gmm_component& gmm_component) {
    gmm_component_ = gmm_component;
    updateUiFromComponent();
}

void GmmComponentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        gmm_component_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void GmmComponentDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void GmmComponentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->meanEdit->setReadOnly(readOnly);
    ui_->stdevEdit->setReadOnly(readOnly);
    ui_->weightEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void GmmComponentDetailDialog::updateUiFromComponent() {
    ui_->componentIndexEdit->setValue(gmm_component_.component_index);
    ui_->descriptionEdit->setText(QString::fromStdString(gmm_component_.description));
    ui_->meanEdit->setText(QString::number(gmm_component_.mean));
    ui_->stdevEdit->setText(QString::number(gmm_component_.stdev));
    ui_->weightEdit->setText(QString::number(gmm_component_.weight));

    populateProvenance(gmm_component_.version,
                       gmm_component_.modified_by,
                       gmm_component_.performed_by,
                       gmm_component_.recorded_at,
                       gmm_component_.change_reason_code,
                       gmm_component_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void GmmComponentDetailDialog::updateComponentFromUi() {
    gmm_component_.component_index = ui_->componentIndexEdit->value();
    gmm_component_.description = ui_->descriptionEdit->text().trimmed().toStdString();
    gmm_component_.mean = ui_->meanEdit->text().trimmed().toDouble();
    gmm_component_.stdev = ui_->stdevEdit->text().trimmed().toDouble();
    gmm_component_.weight = ui_->weightEdit->text().trimmed().toDouble();
    gmm_component_.modified_by = username_;
}

void GmmComponentDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void GmmComponentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void GmmComponentDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool GmmComponentDetailDialog::validateInput() {
    const QString mean_val = ui_->meanEdit->text().trimmed();
    const QString stdev_val = ui_->stdevEdit->text().trimmed();
    const QString weight_val = ui_->weightEdit->text().trimmed();

    return true && !mean_val.isEmpty() && !stdev_val.isEmpty() && !weight_val.isEmpty();
}

void GmmComponentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save GMM component while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }

    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    gmm_component_.change_reason_code = crSel->reason_code;
    gmm_component_.change_commentary = crSel->commentary;

    updateComponentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving GMM component: "
                              << boost::uuids::to_string(gmm_component_.id);

    QPointer<GmmComponentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, gmm_component = gmm_component_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::save_gmm_component_request request;
        request.data = gmm_component;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "GMM Component saved successfully";
            QString code = QString::fromStdString(boost::uuids::to_string(self->gmm_component_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->gmm_componentSaved(code);
            self->notifySaveSuccess(tr("GMM Component '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void GmmComponentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete GMM component while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(gmm_component_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete GMM Component",
        QString("Are you sure you want to delete GMM component '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(ChangeReasonDialog::OperationType::Delete, false);
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting GMM component: "
                              << boost::uuids::to_string(gmm_component_.id);

    QPointer<GmmComponentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(gmm_component_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::delete_gmm_component_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "GMM Component deleted successfully";
            emit self->statusMessage(QString("GMM Component '%1' deleted").arg(code));
            emit self->gmm_componentDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
