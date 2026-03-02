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
#include "ores.qt/ReportDefinitionDetailDialog.hpp"

#include <QMessageBox>
#include <QComboBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_ReportDefinitionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.reporting/messaging/report_definition_protocol.hpp"
#include "ores.reporting/messaging/report_type_protocol.hpp"
#include "ores.reporting/messaging/concurrency_policy_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportDefinitionDetailDialog::ReportDefinitionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::ReportDefinitionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

ReportDefinitionDetailDialog::~ReportDefinitionDetailDialog() {
    delete ui_;
}

QTabWidget* ReportDefinitionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ReportDefinitionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ReportDefinitionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void ReportDefinitionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ReportDefinitionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &ReportDefinitionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &ReportDefinitionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &ReportDefinitionDetailDialog::onCloseClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &ReportDefinitionDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &ReportDefinitionDetailDialog::onFieldChanged);
    connect(ui_->reportTypeCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
            &ReportDefinitionDetailDialog::onFieldChanged);
    connect(ui_->cronWidget, &CronExpressionWidget::cronChanged, this,
            &ReportDefinitionDetailDialog::onFieldChanged);
    connect(ui_->concurrencyPolicyCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
            &ReportDefinitionDetailDialog::onFieldChanged);
}

void ReportDefinitionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    loadReportTypes();
    loadConcurrencyPolicies();
}

void ReportDefinitionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ReportDefinitionDetailDialog::setDefinition(
    const reporting::domain::report_definition& definition) {
    definition_ = definition;
    updateUiFromDefinition();
}

void ReportDefinitionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        definition_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void ReportDefinitionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->reportTypeCombo->setEnabled(!readOnly);
    ui_->cronWidget->setReadOnly(readOnly);
    ui_->concurrencyPolicyCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ReportDefinitionDetailDialog::loadReportTypes() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<ReportDefinitionDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<reporting::domain::report_type> types;
        std::string error;
    };

    auto task = [self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {false, {}, "Dialog closed"};

        reporting::messaging::get_report_types_request request;
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));

        if (!result)
            return {false, {}, "Failed to fetch report types"};

        return {true, std::move(result->types), {}};
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (self && result.success) {
            self->populateReportTypeCombo(result.types);
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void ReportDefinitionDetailDialog::loadConcurrencyPolicies() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<ReportDefinitionDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<reporting::domain::concurrency_policy> policies;
        std::string error;
    };

    auto task = [self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {false, {}, "Dialog closed"};

        reporting::messaging::get_concurrency_policies_request request;
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));

        if (!result)
            return {false, {}, "Failed to fetch concurrency policies"};

        return {true, std::move(result->policies), {}};
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (self && result.success) {
            self->populateConcurrencyPolicyCombo(result.policies);
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void ReportDefinitionDetailDialog::populateReportTypeCombo(
    const std::vector<reporting::domain::report_type>& types) {

    ui_->reportTypeCombo->blockSignals(true);
    ui_->reportTypeCombo->clear();
    for (const auto& t : types) {
        ui_->reportTypeCombo->addItem(
            QString::fromStdString(t.name),
            QString::fromStdString(t.code));
    }
    ui_->reportTypeCombo->blockSignals(false);

    // Re-apply current definition value if set
    if (!definition_.report_type.empty()) {
        const QString code = QString::fromStdString(definition_.report_type);
        for (int i = 0; i < ui_->reportTypeCombo->count(); ++i) {
            if (ui_->reportTypeCombo->itemData(i).toString() == code) {
                ui_->reportTypeCombo->setCurrentIndex(i);
                break;
            }
        }
    }
}

void ReportDefinitionDetailDialog::populateConcurrencyPolicyCombo(
    const std::vector<reporting::domain::concurrency_policy>& policies) {

    ui_->concurrencyPolicyCombo->blockSignals(true);
    ui_->concurrencyPolicyCombo->clear();
    for (const auto& p : policies) {
        ui_->concurrencyPolicyCombo->addItem(
            QString::fromStdString(p.name),
            QString::fromStdString(p.code));
    }
    ui_->concurrencyPolicyCombo->blockSignals(false);

    // Re-apply current definition value if set
    if (!definition_.concurrency_policy.empty()) {
        const QString code = QString::fromStdString(definition_.concurrency_policy);
        for (int i = 0; i < ui_->concurrencyPolicyCombo->count(); ++i) {
            if (ui_->concurrencyPolicyCombo->itemData(i).toString() == code) {
                ui_->concurrencyPolicyCombo->setCurrentIndex(i);
                break;
            }
        }
    }
}

void ReportDefinitionDetailDialog::updateUiFromDefinition() {
    ui_->nameEdit->setText(QString::fromStdString(definition_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(definition_.description));

    // Report type combo
    if (!definition_.report_type.empty()) {
        const QString code = QString::fromStdString(definition_.report_type);
        for (int i = 0; i < ui_->reportTypeCombo->count(); ++i) {
            if (ui_->reportTypeCombo->itemData(i).toString() == code) {
                ui_->reportTypeCombo->setCurrentIndex(i);
                break;
            }
        }
    }

    // Cron widget
    ui_->cronWidget->setCronExpression(
        QString::fromStdString(definition_.schedule_expression));

    // Concurrency policy combo
    if (!definition_.concurrency_policy.empty()) {
        const QString code = QString::fromStdString(definition_.concurrency_policy);
        for (int i = 0; i < ui_->concurrencyPolicyCombo->count(); ++i) {
            if (ui_->concurrencyPolicyCombo->itemData(i).toString() == code) {
                ui_->concurrencyPolicyCombo->setCurrentIndex(i);
                break;
            }
        }
    }

    populateProvenance(definition_.version,
                       definition_.modified_by,
                       definition_.performed_by,
                       definition_.recorded_at,
                       definition_.change_reason_code,
                       definition_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ReportDefinitionDetailDialog::updateDefinitionFromUi() {
    if (createMode_) {
        definition_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    definition_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    definition_.report_type = ui_->reportTypeCombo->currentData().toString().toStdString();
    definition_.schedule_expression = ui_->cronWidget->cronExpression().trimmed().toStdString();
    definition_.concurrency_policy = ui_->concurrencyPolicyCombo->currentData().toString().toStdString();
    definition_.modified_by = username_;
    definition_.performed_by = username_;
}

void ReportDefinitionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ReportDefinitionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ReportDefinitionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ReportDefinitionDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();
    if (name_val.isEmpty())
        return false;
    if (ui_->reportTypeCombo->currentIndex() < 0)
        return false;
    if (!ui_->cronWidget->isValid())
        return false;
    if (ui_->concurrencyPolicyCombo->currentIndex() < 0)
        return false;
    return true;
}

void ReportDefinitionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save report definition while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateDefinitionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving report definition: " << definition_.name;

    QPointer<ReportDefinitionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, definition = definition_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::save_report_definition_request request;
        request.definition = definition;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_report_definition_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = reporting::messaging::save_report_definition_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Report Definition saved successfully";
            QString code = QString::fromStdString(self->definition_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->definitionSaved(code);
            self->notifySaveSuccess(tr("Report Definition '%1' saved").arg(code));
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

void ReportDefinitionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete report definition while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(definition_.name);
    auto reply = MessageBoxHelper::question(this, "Delete Report Definition",
        QString("Are you sure you want to delete report definition '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting report definition: " << definition_.name;

    QPointer<ReportDefinitionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = definition_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::delete_report_definition_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_report_definition_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = reporting::messaging::delete_report_definition_response::
            deserialize(*payload_result);

        if (!response || response->results.empty()) {
            return {false, "Invalid server response"};
        }

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Report Definition deleted successfully";
            emit self->statusMessage(QString("Report Definition '%1' deleted").arg(code));
            emit self->definitionDeleted(code);
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
