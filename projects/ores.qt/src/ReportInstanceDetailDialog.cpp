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
#include "ores.qt/ReportInstanceDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ui_ReportInstanceDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.reporting/messaging/report_instance_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportInstanceDetailDialog::ReportInstanceDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::ReportInstanceDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

ReportInstanceDetailDialog::~ReportInstanceDetailDialog() {
    delete ui_;
}

QTabWidget* ReportInstanceDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ReportInstanceDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ReportInstanceDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void ReportInstanceDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ReportInstanceDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &ReportInstanceDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &ReportInstanceDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &ReportInstanceDetailDialog::onCloseClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &ReportInstanceDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &ReportInstanceDetailDialog::onFieldChanged);
    connect(ui_->outputEdit, &QPlainTextEdit::textChanged, this,
            &ReportInstanceDetailDialog::onFieldChanged);
}

void ReportInstanceDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ReportInstanceDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ReportInstanceDetailDialog::setInstance(
    const reporting::domain::report_instance& instance) {
    instance_ = instance;
    updateUiFromInstance();
}

void ReportInstanceDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        instance_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void ReportInstanceDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->outputEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ReportInstanceDetailDialog::updateUiFromInstance() {
    ui_->nameEdit->setText(QString::fromStdString(instance_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(instance_.description));
    ui_->outputEdit->setPlainText(QString::fromStdString(instance_.output_message));

    populateProvenance(instance_.version,
                       instance_.modified_by,
                       instance_.performed_by,
                       instance_.recorded_at,
                       instance_.change_reason_code,
                       instance_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ReportInstanceDetailDialog::updateInstanceFromUi() {
    if (createMode_) {
        instance_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    instance_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instance_.output_message = ui_->outputEdit->toPlainText().trimmed().toStdString();
    instance_.modified_by = username_;
    instance_.performed_by = username_;
}

void ReportInstanceDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ReportInstanceDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ReportInstanceDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ReportInstanceDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !name_val.isEmpty();
}

void ReportInstanceDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save report instance while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateInstanceFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving report instance: " << instance_.name;

    QPointer<ReportInstanceDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instance = instance_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::save_report_instance_request request;
        request.instance = instance;
        auto response_result = self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }


        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Report Instance saved successfully";
            QString code = QString::fromStdString(self->instance_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->instanceSaved(code);
            self->notifySaveSuccess(tr("Report Instance '%1' saved").arg(code));
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

void ReportInstanceDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete report instance while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(instance_.name);
    auto reply = MessageBoxHelper::question(this, "Delete Report Instance",
        QString("Are you sure you want to delete report instance '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting report instance: " << instance_.name;

    QPointer<ReportInstanceDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = instance_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::delete_report_instance_request request;
        request.ids.push_back(boost::uuids::to_string(id));
        auto response_result = self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Report Instance deleted successfully";
            emit self->statusMessage(QString("Report Instance '%1' deleted").arg(code));
            emit self->instanceDeleted(code);
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
