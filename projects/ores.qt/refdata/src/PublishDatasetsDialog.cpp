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
#include "ores.qt/PublishDatasetsDialog.hpp"
#include "ores.dq.api/messaging/dataset_dependency_protocol.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include <QFormLayout>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QTimer>
#include <QUuid>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace {
inline auto& lg() {
    static auto instance = ores::logging::make_logger("ores.qt.publish_datasets_dialog");
    return instance;
}
} // namespace

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// PublishDatasetsDialog (Main Wizard)
// ============================================================================

PublishDatasetsDialog::PublishDatasetsDialog(ClientManager* clientManager,
                                             const QString& username,
                                             QWidget* parent)
    : QWizard(parent)
    , clientManager_(clientManager)
    , username_(username) {

    setWindowTitle(tr("Publish Datasets"));
    setMinimumSize(600, 500);
    resize(700, 550);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void PublishDatasetsDialog::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Selection, new SelectionPage(this));
    setPage(Page_Options, new OptionsPage(this));
    setPage(Page_Review, new ReviewPage(this));
    setPage(Page_Progress, new ProgressPage(this));
    setPage(Page_Results, new ResultsPage(this));

    setStartId(Page_Selection);
}

void PublishDatasetsDialog::setDatasets(const std::vector<dq::domain::dataset>& datasets) {

    datasets_ = datasets;

    requestedIds_.clear();
    for (const auto& ds : datasets_) {
        requestedIds_.push_back(boost::uuids::to_string(ds.id));
    }
}

dq::domain::publication_mode PublishDatasetsDialog::selectedMode() const {
    auto* optionsPage = qobject_cast<OptionsPage*>(page(Page_Options));
    if (!optionsPage) {
        return dq::domain::publication_mode::upsert;
    }

    auto modeStr = optionsPage->modeCombo()->currentData().toString().toStdString();
    auto mode = dq::domain::publication_mode_from_string(modeStr);
    return mode.value_or(dq::domain::publication_mode::upsert);
}

bool PublishDatasetsDialog::resolveDependencies() const {
    auto* optionsPage = qobject_cast<OptionsPage*>(page(Page_Options));
    if (!optionsPage) {
        return true;
    }
    return optionsPage->resolveDependenciesCheck()->isChecked();
}

// ============================================================================
// SelectionPage
// ============================================================================

SelectionPage::SelectionPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Selected Datasets"));
    setSubTitle(tr("Review the datasets you have selected for publication."));

    auto* layout = new QVBoxLayout(this);

    countLabel_ = new QLabel(this);
    countLabel_->setStyleSheet("font-weight: bold; font-size: 14px;");
    layout->addWidget(countLabel_);

    layout->addSpacing(10);

    datasetList_ = new QListWidget(this);
    datasetList_->setSelectionMode(QAbstractItemView::NoSelection);
    datasetList_->setAlternatingRowColors(true);
    layout->addWidget(datasetList_, 1);
}

void SelectionPage::initializePage() {
    datasetList_->clear();

    const auto& datasets = wizard_->datasets();
    for (const auto& ds : datasets) {
        auto* item = new QListWidgetItem(QString::fromStdString(ds.code) + " - " +
                                         QString::fromStdString(ds.name));
        datasetList_->addItem(item);
    }

    countLabel_->setText(tr("%1 dataset(s) selected for publication").arg(datasets.size()));
}

// ============================================================================
// OptionsPage
// ============================================================================

OptionsPage::OptionsPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Publication Options"));
    setSubTitle(tr("Configure how the datasets should be published."));

    auto* layout = new QFormLayout(this);
    layout->setSpacing(16);

    // Publication mode
    modeCombo_ = new QComboBox(this);
    modeCombo_->addItem(tr("Upsert (insert new, update existing)"), "upsert");
    modeCombo_->addItem(tr("Insert Only (skip existing)"), "insert_only");
    modeCombo_->addItem(tr("Replace All (delete then insert)"), "replace_all");
    layout->addRow(tr("Publication Mode:"), modeCombo_);

    // Mode description
    auto* modeDescription =
        new QLabel(tr("<i>Upsert</i> will insert new records and update existing ones.<br>"
                      "<i>Insert Only</i> will only add new records, skipping existing.<br>"
                      "<i>Replace All</i> will delete all existing records first, then insert."),
                   this);
    modeDescription->setWordWrap(true);
    layout->addRow("", modeDescription);

    // Dependencies
    resolveDependenciesCheck_ = new QCheckBox(tr("Automatically publish dependencies first"), this);
    resolveDependenciesCheck_->setChecked(true);
    layout->addRow(tr("Dependencies:"), resolveDependenciesCheck_);

    auto* dependencyDescription =
        new QLabel(tr("When enabled, any datasets that this dataset depends on will be "
                      "published first in the correct order."),
                   this);
    dependencyDescription->setWordWrap(true);
    layout->addRow("", dependencyDescription);

    // Register fields for access from other pages
    registerField("publicationMode", modeCombo_);
    registerField("resolveDependencies", resolveDependenciesCheck_);
}

// ============================================================================
// ReviewPage
// ============================================================================

ReviewPage::ReviewPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Review Publication Order"));
    setSubTitle(tr("Verify the order in which datasets will be published."));

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(this);
    layout->addWidget(statusLabel_);

    layout->addSpacing(10);

    summaryLabel_ = new QLabel(this);
    summaryLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(summaryLabel_);

    layout->addSpacing(10);

    orderTable_ = new QTableWidget(this);
    orderTable_->setColumnCount(3);
    orderTable_->setHorizontalHeaderLabels({tr("Order"), tr("Dataset Code"), tr("Type")});
    orderTable_->horizontalHeader()->setStretchLastSection(true);
    orderTable_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    orderTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    orderTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    orderTable_->setAlternatingRowColors(true);
    layout->addWidget(orderTable_, 1);
}

void ReviewPage::initializePage() {
    resolved_ = false;
    statusLabel_->setText(tr("Resolving dependencies..."));
    orderTable_->setRowCount(0);
    summaryLabel_->clear();

    // Resolve dependencies after a short delay to show the UI
    QTimer::singleShot(100, this, &ReviewPage::resolveDependencies);
}

bool ReviewPage::isComplete() const {
    return resolved_;
}

void ReviewPage::resolveDependencies() {
    bool resolve = wizard_->resolveDependencies();

    if (!resolve) {
        wizard_->resolvedDatasets() = wizard_->datasets();
        wizard_->requestedIds().clear();
        for (const auto& ds : wizard_->datasets()) {
            wizard_->requestedIds().push_back(boost::uuids::to_string(ds.id));
        }
        resolved_ = true;
        statusLabel_->setText(tr("Ready to publish."));
        updatePublicationOrder();
        emit completeChanged();
        return;
    }

    // Build request
    dq::messaging::resolve_dependencies_request request;
    for (const auto& ds : wizard_->datasets()) {
        request.dataset_ids.push_back(boost::uuids::to_string(ds.id));
    }

    // Send request
    auto result = wizard_->clientManager()->process_authenticated_request(std::move(request));

    if (!result) {
        wizard_->resolvedDatasets() = wizard_->datasets();
        wizard_->requestedIds().clear();
        for (const auto& ds : wizard_->datasets()) {
            wizard_->requestedIds().push_back(boost::uuids::to_string(ds.id));
        }
        resolved_ = true;
        const auto& errMsg = result.error();
        QString errorDetail = errMsg.empty() ? tr("unknown error") : QString::fromStdString(errMsg);
        statusLabel_->setText(
            tr("Failed to resolve dependencies (%1). Will publish selected datasets only.")
                .arg(errorDetail));
        updatePublicationOrder();
        emit completeChanged();
        return;
    }

    wizard_->resolvedDatasets() = std::move(result->datasets);
    wizard_->requestedIds() = std::move(result->requested_ids);

    resolved_ = true;
    statusLabel_->setText(tr("Dependencies resolved successfully."));
    updatePublicationOrder();
    emit completeChanged();
}

void ReviewPage::updatePublicationOrder() {
    const auto& resolvedDatasets = wizard_->resolvedDatasets();
    const auto& requestedIds = wizard_->requestedIds();

    orderTable_->setRowCount(static_cast<int>(resolvedDatasets.size()));

    int dependencyCount = 0;
    int requestedCount = 0;

    for (int row = 0; row < static_cast<int>(resolvedDatasets.size()); ++row) {
        const auto& ds = resolvedDatasets[row];

        // Order column
        auto* orderItem = new QTableWidgetItem(QString::number(row + 1));
        orderItem->setTextAlignment(Qt::AlignCenter);
        orderTable_->setItem(row, 0, orderItem);

        // Code column
        auto* codeItem = new QTableWidgetItem(QString::fromStdString(ds.code));
        orderTable_->setItem(row, 1, codeItem);

        // Type column - check if this is a dependency or requested
        bool isRequested =
            std::ranges::find(requestedIds, boost::uuids::to_string(ds.id)) != requestedIds.end();
        QString typeText = isRequested ? tr("Selected") : tr("Dependency");
        auto* typeItem = new QTableWidgetItem(typeText);

        if (!isRequested) {
            QFont font = typeItem->font();
            font.setItalic(true);
            typeItem->setFont(font);
            ++dependencyCount;
        } else {
            ++requestedCount;
        }
        orderTable_->setItem(row, 2, typeItem);
    }

    if (dependencyCount > 0) {
        summaryLabel_->setText(tr("%1 dataset(s) will be published: %2 selected, %3 dependencies")
                                   .arg(resolvedDatasets.size())
                                   .arg(requestedCount)
                                   .arg(dependencyCount));
    } else {
        summaryLabel_->setText(tr("%1 dataset(s) will be published").arg(resolvedDatasets.size()));
    }
}

// ============================================================================
// ProgressPage
// ============================================================================

ProgressPage::ProgressPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Publishing"));
    setSubTitle(tr("Please wait while the datasets are being published."));

    auto* layout = new QVBoxLayout(this);
    layout->addStretch();

    statusLabel_ = new QLabel(tr("Starting publication..."), this);
    statusLabel_->setAlignment(Qt::AlignCenter);
    statusLabel_->setStyleSheet("font-size: 14px;");
    layout->addWidget(statusLabel_);

    layout->addSpacing(20);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0); // Indeterminate
    progressBar_->setMinimumWidth(400);
    layout->addWidget(progressBar_, 0, Qt::AlignCenter);

    layout->addStretch();

    setCommitPage(true);
}

void ProgressPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    statusLabel_->setText(tr("Starting publication..."));

    QTimer::singleShot(100, this, &ProgressPage::performPublish);
}

bool ProgressPage::isComplete() const {
    return publishComplete_;
}

int ProgressPage::nextId() const {
    return PublishDatasetsDialog::Page_Results;
}

void ProgressPage::performPublish() {
    const auto& resolvedDatasets = wizard_->resolvedDatasets();

    if (resolvedDatasets.empty()) {
        statusLabel_->setText(tr("No datasets to publish."));
        publishComplete_ = true;
        publishSuccess_ = false;
        emit completeChanged();
        wizard_->next();
        return;
    }

    statusLabel_->setText(
        tr("Submitting publish request for %1 dataset(s)...").arg(resolvedDatasets.size()));

    // Capture values for the background thread
    std::vector<std::string> datasetIds;
    datasetIds.reserve(resolvedDatasets.size());
    for (const auto& ds : resolvedDatasets)
        datasetIds.push_back(boost::uuids::to_string(ds.id));

    const dq::domain::publication_mode mode = wizard_->selectedMode();
    const std::string publishedBy = wizard_->username().toStdString();
    ClientManager* clientManager = wizard_->clientManager();

    using ResponseType = dq::messaging::publish_datasets_response;

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished, [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        auto* resultsPage =
            qobject_cast<ResultsPage*>(wizard()->page(PublishDatasetsDialog::Page_Results));

        if (!result) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to communicate with server for dataset publication.";
            statusLabel_->setText(tr("Publication failed!"));
            publishComplete_ = true;
            publishSuccess_ = false;
            if (resultsPage)
                resultsPage->setResults(false, tr("Failed to communicate with server."));
        } else if (!result->success) {
            BOOST_LOG_SEV(lg(), error) << "Dataset publication failed: " << result->message;
            statusLabel_->setText(tr("Publication failed!"));
            publishComplete_ = true;
            publishSuccess_ = false;
            if (resultsPage)
                resultsPage->setResults(false, QString::fromStdString(result->message));
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Dataset publish workflow started: instance=" << result->instance_id
                << " datasets=" << result->datasets_dispatched;
            statusLabel_->setText(tr("Publication workflow started!"));
            publishComplete_ = true;
            publishSuccess_ = true;
            wizard_->instanceId() = result->instance_id;
            wizard_->datasetsDispatched() = result->datasets_dispatched;
            if (resultsPage)
                resultsPage->setResults(true, QString());
        }

        emit completeChanged();
        if (publishComplete_)
            wizard_->next();
    });

    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, datasetIds, mode, publishedBy]() -> std::optional<ResponseType> {
            dq::messaging::publish_datasets_request request;
            request.dataset_ids = datasetIds;
            request.mode = mode;
            request.published_by = publishedBy;
            request.resolve_dependencies = false; // Already resolved by ReviewPage

            auto result = clientManager->process_authenticated_request(std::move(request));
            if (!result)
                return std::nullopt;
            return *result;
        });

    watcher->setFuture(future);
}

// ============================================================================
// ResultsPage
// ============================================================================

ResultsPage::ResultsPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Publication Results"));
    setSubTitle(tr("Summary of the publication operation."));
    setFinalPage(true);

    auto* layout = new QVBoxLayout(this);

    overallStatusLabel_ = new QLabel(this);
    overallStatusLabel_->setWordWrap(true);
    overallStatusLabel_->setStyleSheet("font-size: 14px; font-weight: bold;");
    layout->addWidget(overallStatusLabel_);

    layout->addSpacing(10);

    stepsWidget_ = new WorkflowStepsWidget(wizard_->clientManager(), this);
    stepsWidget_->setVisible(false);
    layout->addWidget(stepsWidget_, 1);

    connect(stepsWidget_,
            &WorkflowStepsWidget::instanceReachedTerminalState,
            this,
            &ResultsPage::onWorkflowComplete);
}

void ResultsPage::setResults(bool success, const QString& errorMessage) {
    overallSuccess_ = success;
    errorMessage_ = errorMessage;
}

void ResultsPage::initializePage() {
    workflowComplete_ = false;
    const auto& instanceId = wizard_->instanceId();

    if (overallSuccess_) {
        overallStatusLabel_->setText(
            tr("Publication workflow started — waiting for completion..."));
        overallStatusLabel_->setStyleSheet("font-size: 14px; font-weight: bold;");
        if (!instanceId.empty()) {
            stepsWidget_->setVisible(true);
            stepsWidget_->setInstance(QUuid::fromString(QString::fromStdString(instanceId)));
        } else {
            workflowComplete_ = true;
        }
    } else {
        QString msg = tr("Publication failed.");
        if (!errorMessage_.isEmpty())
            msg += " " + errorMessage_;
        overallStatusLabel_->setText(msg);
        overallStatusLabel_->setStyleSheet("font-size: 14px; font-weight: bold; color: #cc0000;");
        workflowComplete_ = true;
    }

    emit completeChanged();
}

bool ResultsPage::isComplete() const {
    return workflowComplete_;
}

void ResultsPage::onWorkflowComplete(bool success) {
    if (success) {
        overallStatusLabel_->setText(tr("Publication workflow completed successfully."));
        overallStatusLabel_->setStyleSheet("font-size: 14px; font-weight: bold; color: #228B22;");

        QStringList publishedCodes;
        for (const auto& ds : wizard_->resolvedDatasets())
            publishedCodes.append(QString::fromStdString(ds.code));
        emit qobject_cast<PublishDatasetsDialog*>(wizard())->datasetsPublished(publishedCodes);
    } else {
        overallStatusLabel_->setText(tr("Publication workflow completed with errors."));
        overallStatusLabel_->setStyleSheet("font-size: 14px; font-weight: bold; color: #cc0000;");
    }
    workflowComplete_ = true;
    emit completeChanged();
}

}
