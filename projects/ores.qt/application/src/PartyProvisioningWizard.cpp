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
#include "ores.qt/PartyProvisioningWizard.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.qt.headless/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"
#include <QFutureWatcher>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QSizePolicy>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// PartyProvisioningWizard
// ============================================================================

PartyProvisioningWizard::PartyProvisioningWizard(ClientManager* clientManager, QWidget* parent)
    : QWizard(parent)
    , clientManager_(clientManager) {

    setWindowTitle(tr("Party Setup"));
    setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::BuildingSkyscraper, IconUtils::DefaultIconColor));
    setMinimumSize(900, 700);
    resize(900, 700);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void PartyProvisioningWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome, new PartyWelcomePage(this));
    setPage(Page_CounterpartySetup, new PartyCounterpartySetupPage(this));
    setPage(Page_Execute, new PartyExecutePage(this));
    setPage(Page_Summary, new PartyApplyAndSummaryPage(this));

    setStartId(Page_Welcome);
}

// ============================================================================
// PartyWelcomePage
// ============================================================================

PartyWelcomePage::PartyWelcomePage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Welcome"));
    setupUI();
}

void PartyWelcomePage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* welcomeLabel = new QLabel(tr("Welcome to the party setup wizard."), this);
    welcomeLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    welcomeLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(welcomeLabel);

    layout->addSpacing(10);

    auto* descLabel = new QLabel(this);
    descLabel->setWordWrap(true);
    descLabel->setText(tr("This wizard sets up the operational structure for this party. "
                          "The party hierarchy already exists — this wizard will import "
                          "counterparties and configure the organisational structure.\n\n"
                          "The setup process includes:"));
    layout->addWidget(descLabel);

    auto* stepsLabel = new QLabel(this);
    stepsLabel->setWordWrap(true);
    stepsLabel->setTextFormat(Qt::RichText);
    stepsLabel->setText(tr("<ol>"
                           "<li><b>Counterparty Setup</b> - Select dataset size for importing "
                           "counterparties from the GLEIF LEI registry.</li>"
                           "<li><b>Execute</b> - All data is published and configured in one "
                           "step with live progress tracking.</li>"
                           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    auto* noteBox = new QGroupBox(tr("Note"), this);
    auto* noteLayout = new QVBoxLayout(noteBox);
    auto* noteLabel =
        new QLabel(tr("You can skip this setup by clicking Cancel. You can configure "
                      "counterparties and reports manually from the application menus."),
                   this);
    noteLabel->setWordWrap(true);
    noteLayout->addWidget(noteLabel);
    layout->addWidget(noteBox);
}

// ============================================================================
// PartyCounterpartySetupPage
// ============================================================================

PartyCounterpartySetupPage::PartyCounterpartySetupPage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Counterparty Import"));
    setSubTitle(tr("Select the GLEIF dataset size to use for counterparty import."));
    setupUI();
}

void PartyCounterpartySetupPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* infoLabel = new QLabel(this);
    infoLabel->setWordWrap(true);
    infoLabel->setText(tr("Counterparties represent the external entities your organisation "
                          "trades with or has business relationships with.\n\n"
                          "Counterparties are imported from the full GLEIF LEI registry dataset. "
                          "Select the dataset size to use for the import.\n\n"
                          "You can also add counterparties manually from the Counterparties "
                          "window after completing this wizard."));
    infoLabel->setTextFormat(Qt::RichText);
    layout->addWidget(infoLabel);

    auto* sizeLayout = new QHBoxLayout();
    sizeLayout->addWidget(new QLabel(tr("Dataset size:"), this));
    datasetSizeCombo_ = new QComboBox(this);
    datasetSizeCombo_->addItem(tr("Large (~15,000 entities)"), QStringLiteral("large"));
    datasetSizeCombo_->addItem(tr("Small (~6,000 entities)"), QStringLiteral("small"));
    sizeLayout->addWidget(datasetSizeCombo_);
    sizeLayout->addStretch();
    layout->addLayout(sizeLayout);

    layout->addStretch();
}

bool PartyCounterpartySetupPage::validatePage() {
    wizard_->setLeiDatasetSize(datasetSizeCombo_->currentData().toString());
    return true;
}

// ============================================================================
// PartyExecutePage
// ============================================================================

PartyExecutePage::PartyExecutePage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Setting Up Party"));
    setSubTitle(tr("Importing counterparties and configuring organisation structure. "
                   "Please wait — this may take several minutes."));
    setCommitPage(true); // Disable Back once execution starts to prevent re-running.
    setFinalPage(false);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);
    progressBar_->setTextVisible(false);
    progressBar_->setStyleSheet("QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
                                "background: #2d2d2d; height: 20px; }"
                                "QProgressBar::chunk { background-color: #4a9eff; }");
    layout->addWidget(progressBar_);

    stepsWidget_ = new WorkflowStepsWidget(wizard_->clientManager(), this);
    stepsWidget_->setVisible(false);
    stepsWidget_->setMinimumHeight(200);
    stepsWidget_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    layout->addWidget(stepsWidget_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setFont(FontUtils::monospace());
    logOutput_->setMaximumHeight(120);
    layout->addWidget(logOutput_);
}

bool PartyExecutePage::isComplete() const {
    return allComplete_;
}

void PartyExecutePage::initializePage() {
    allComplete_ = false;
    allSuccess_ = false;
    publishedBy_ = wizard_->clientManager()->currentUsername();

    logOutput_->clear();
    stepsWidget_->setVisible(false);
    progressBar_->setRange(0, 0);
    progressBar_->setVisible(true);
    progressBar_->setStyleSheet("QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
                                "background: #2d2d2d; height: 20px; }"
                                "QProgressBar::chunk { background-color: #4a9eff; }");
    statusLabel_->setStyleSheet("font-weight: bold;");

    // Disconnect any previous workflow connection to avoid duplicate signals.
    disconnect(stepsWidget_, &WorkflowStepsWidget::instanceReachedTerminalState, this, nullptr);

    startCounterpartyPublish();
}

void PartyExecutePage::appendLog(const QString& msg) {
    logOutput_->append(msg);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void PartyExecutePage::markFailed(const QString& errorMsg) {
    BOOST_LOG_SEV(lg(), error) << "Party setup failed: " << errorMsg.toStdString();
    statusLabel_->setText(tr("Setup failed: %1").arg(errorMsg));
    statusLabel_->setStyleSheet("font-weight: bold; color: #cc0000;");
    progressBar_->setRange(0, 1);
    progressBar_->setValue(1);
    progressBar_->setStyleSheet("QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
                                "background: #2d2d2d; height: 20px; }"
                                "QProgressBar::chunk { background-color: #cc0000; }");
    appendLog(tr("ERROR: %1").arg(errorMsg));
    allSuccess_ = false;
    allComplete_ = true;
    emit completeChanged();
}

// Phase 1: Publish counterparties only (opted_in_datasets filter targets the selected size).
void PartyExecutePage::startCounterpartyPublish() {
    ClientManager* clientManager = wizard_->clientManager();
    const std::string selectedBundle = wizard_->selectedBundleCode().toStdString();
    const std::string size = wizard_->leiDatasetSize().toStdString().empty() ?
                                 "small" :
                                 wizard_->leiDatasetSize().toStdString();
    const std::string datasetCode = "gleif.lei_counterparties." + size;

    statusLabel_->setText(
        tr("Importing counterparties (%1 dataset)...").arg(wizard_->leiDatasetSize()));
    appendLog(tr("Phase 1: importing counterparties from dataset: %1")
                  .arg(QString::fromStdString(datasetCode)));
    BOOST_LOG_SEV(lg(), info) << "Phase 1: counterparty publish, dataset=" << datasetCode;

    struct BundleResult {
        bool success = false;
        std::string error_message;
        std::string instance_id;
        int datasets_dispatched = 0;
    };

    auto* watcher = new QFutureWatcher<BundleResult>(this);
    connect(watcher, &QFutureWatcher<BundleResult>::finished, this, [this, watcher]() {
        BundleResult result;
        try {
            result = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Phase 1 async task threw: " << e.what();
            result.error_message = e.what();
        }
        watcher->deleteLater();

        if (!result.success) {
            markFailed(QString::fromStdString(result.error_message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Counterparty workflow started: instance="
                                  << result.instance_id;
        appendLog(tr("Counterparty workflow started: %1 dataset(s) dispatched.")
                      .arg(result.datasets_dispatched));
        statusLabel_->setText(tr("Importing counterparties..."));

        progressBar_->setVisible(false);
        stepsWidget_->setVisible(true);

        // Connect for this workflow phase.
        connect(stepsWidget_,
                &WorkflowStepsWidget::instanceReachedTerminalState,
                this,
                &PartyExecutePage::onCounterpartyWorkflowComplete,
                Qt::SingleShotConnection);
        stepsWidget_->setInstance(QUuid::fromString(QString::fromStdString(result.instance_id)));
        stepsWidget_->preSeed(result.datasets_dispatched);
    });

    QFuture<BundleResult> future = QtConcurrent::run(
        [clientManager, selectedBundle, datasetCode, publishedBy = publishedBy_]() -> BundleResult {
            BundleResult result;
            dq::messaging::publish_bundle_params params;
            params.opted_in_datasets.push_back(datasetCode);
            const std::string paramsJson = dq::messaging::build_params_json(params);

            dq::messaging::publish_bundle_request request;
            request.bundle_code = selectedBundle;
            request.mode = dq::domain::publication_mode::upsert;
            request.published_by = publishedBy;
            request.atomic = true;
            request.params_json = paramsJson;

            auto resp = clientManager->process_authenticated_request(std::move(request),
                                                                     std::chrono::minutes(5));

            if (!resp) {
                result.error_message = "Failed to communicate with server (counterparty publish)";
                return result;
            }
            if (!resp->success) {
                result.error_message = resp->error_message;
                return result;
            }
            result.success = true;
            result.instance_id = resp->instance_id;
            result.datasets_dispatched = resp->datasets_dispatched;
            return result;
        });

    watcher->setFuture(future);
}

void PartyExecutePage::onCounterpartyWorkflowComplete(bool success) {
    // Leave stepsWidget_ visible; show spinner for transition to Phase 2.
    progressBar_->setRange(0, 0);
    progressBar_->setVisible(true);

    if (!success) {
        markFailed(tr("Counterparty import workflow completed with errors."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Phase 1 complete: counterparty workflow succeeded";
    appendLog(tr("Counterparties imported successfully."));
    bundleQueue_ = ores::dq::messaging::party_provisioning_bundle_plan();
    bundleIndex_ = 0;
    startNextPartyBundle();
}

// Phase 2+: publish every party-scoped bundle in the shared provisioning
// plan (ores::dq::messaging::party_provisioning_bundle_plan()), each in
// full -- no dataset-level filter -- so a new bundle member (e.g. a new
// asset class under synthetic_realistic_2026) never requires a client code
// change, only a new row in dq_dataset_bundle_member_populate.sql.
void PartyExecutePage::startNextPartyBundle() {
    if (bundleIndex_ >= bundleQueue_.size()) {
        statusLabel_->setText(tr("Activating party..."));
        startActivate();
        return;
    }

    ClientManager* clientManager = wizard_->clientManager();
    const auto& step = bundleQueue_[bundleIndex_];
    const QString label = QString::fromStdString(step.label);

    appendLog(tr("Publishing %1...").arg(label));
    statusLabel_->setText(tr("Publishing %1...").arg(label));
    BOOST_LOG_SEV(lg(), info) << "Publishing bundle '" << step.bundle_code << "' (" << step.label
                              << ")";

    struct BundleResult {
        bool success = false;
        std::string error_message;
        std::string instance_id;
        int datasets_dispatched = 0;
    };

    auto* watcher = new QFutureWatcher<BundleResult>(this);
    connect(watcher, &QFutureWatcher<BundleResult>::finished, this, [this, watcher]() {
        BundleResult result;
        try {
            result = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Bundle publish async task threw: " << e.what();
            result.error_message = e.what();
        }
        watcher->deleteLater();

        if (!result.success) {
            markFailed(QString::fromStdString(result.error_message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Bundle workflow started: instance=" << result.instance_id;
        appendLog(
            tr("Workflow started: %1 dataset(s) dispatched.").arg(result.datasets_dispatched));

        progressBar_->setVisible(false);
        stepsWidget_->setVisible(true);

        connect(stepsWidget_,
                &WorkflowStepsWidget::instanceReachedTerminalState,
                this,
                &PartyExecutePage::onPartyBundleWorkflowComplete,
                Qt::SingleShotConnection);
        stepsWidget_->setInstance(QUuid::fromString(QString::fromStdString(result.instance_id)));
        stepsWidget_->preSeed(result.datasets_dispatched);
    });

    QFuture<BundleResult> future =
        QtConcurrent::run([clientManager, step, publishedBy = publishedBy_]() -> BundleResult {
            BundleResult result;
            dq::messaging::publish_bundle_params params;
            params.party_id = boost::uuids::to_string(clientManager->currentPartyId());

            dq::messaging::publish_bundle_request request;
            request.bundle_code = step.bundle_code;
            request.mode = dq::domain::publication_mode::upsert;
            request.published_by = publishedBy;
            request.atomic = true;
            request.params_json = dq::messaging::build_params_json(params);

            auto resp = clientManager->process_authenticated_request(std::move(request),
                                                                     std::chrono::minutes(5));

            if (!resp) {
                result.error_message = "Failed to communicate with server (" + step.label + ")";
                return result;
            }
            if (!resp->success) {
                result.error_message = resp->error_message;
                return result;
            }
            result.success = true;
            result.instance_id = resp->instance_id;
            result.datasets_dispatched = resp->datasets_dispatched;
            return result;
        });

    watcher->setFuture(future);
}

void PartyExecutePage::onPartyBundleWorkflowComplete(bool success) {
    progressBar_->setRange(0, 0);
    progressBar_->setVisible(true);

    const QString label = QString::fromStdString(bundleQueue_[bundleIndex_].label);
    if (!success) {
        markFailed(tr("%1 publish workflow completed with errors.").arg(label));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Bundle complete: " << bundleQueue_[bundleIndex_].bundle_code;
    appendLog(tr("%1 published successfully.").arg(label));
    ++bundleIndex_;
    startNextPartyBundle();
}

// Final phase: mark party status as Active.
void PartyExecutePage::startActivate() {
    ClientManager* clientManager = wizard_->clientManager();

    appendLog(tr("Phase 6: activating party..."));
    BOOST_LOG_SEV(lg(), info) << "Phase 6: marking party active";

    struct ActivateResult {
        bool success = false;
        std::string error;
    };

    auto* watcher = new QFutureWatcher<ActivateResult>(this);
    connect(watcher, &QFutureWatcher<ActivateResult>::finished, this, [this, watcher]() {
        ActivateResult result;
        try {
            result = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Phase 4 async task threw: " << e.what();
            result.error = e.what();
        }
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result.success) {
            // Non-fatal: party may need manual activation.
            BOOST_LOG_SEV(lg(), warn) << "Party activation failed: " << result.error;
            appendLog(tr("Warning: could not activate party: %1")
                          .arg(QString::fromStdString(result.error)));
            appendLog(tr("The party setup wizard may reappear on your next login."));
        } else {
            appendLog(tr("Party activated successfully."));
        }

        statusLabel_->setText(tr("Setup complete!"));
        statusLabel_->setStyleSheet("font-weight: bold; color: #228B22;");
        progressBar_->setStyleSheet("QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
                                    "background: #2d2d2d; height: 20px; }"
                                    "QProgressBar::chunk { background-color: #228B22; }");

        allSuccess_ = result.success;
        allComplete_ = true;
        emit completeChanged();
    });

    QFuture<ActivateResult> future = QtConcurrent::run([clientManager]() -> ActivateResult {
        ActivateResult result;

        const auto party_id = clientManager->currentPartyId();
        BOOST_LOG_SEV(lg(), info) << "Phase 4: activating party_id="
                                  << boost::uuids::to_string(party_id);

        refdata::messaging::get_parties_request list_req;
        list_req.offset = 0;
        list_req.limit = 1000;
        auto list_result = clientManager->process_authenticated_request(std::move(list_req));
        if (!list_result) {
            result.error = "Failed to fetch parties: " + list_result.error();
            BOOST_LOG_SEV(lg(), error) << "Phase 4: " << result.error;
            return result;
        }
        BOOST_LOG_SEV(lg(), debug)
            << "Phase 4: fetched " << list_result->parties.size() << " parties";

        refdata::domain::party party;
        bool found = false;
        for (const auto& p : list_result->parties) {
            if (p.id == party_id) {
                party = p;
                found = true;
                break;
            }
        }
        if (!found) {
            result.error = "Party " + boost::uuids::to_string(party_id) + " not found in list of " +
                           std::to_string(list_result->parties.size()) + " parties";
            BOOST_LOG_SEV(lg(), error) << "Phase 4: " << result.error;
            return result;
        }

        BOOST_LOG_SEV(lg(), info) << "Phase 4: found party '" << party.full_name
                                  << "' current status='" << party.status << "'";

        party.status = "Active";
        party.change_commentary = "Party setup wizard completed";

        refdata::messaging::save_party_request save_req;
        save_req.data = std::move(party);
        auto save_result = clientManager->process_authenticated_request(std::move(save_req));
        if (!save_result || !save_result->success) {
            result.error = save_result ? save_result->message : "no server response";
            BOOST_LOG_SEV(lg(), error) << "Phase 4: save_party_request failed: " << result.error;
            return result;
        }

        BOOST_LOG_SEV(lg(), info) << "Phase 4: party " << boost::uuids::to_string(party_id)
                                  << " marked Active successfully";

        // Write onboarding.party = true, independent of the party's own
        // status — this is what login checks to decide whether to
        // re-launch this wizard, not party.status (which can be flipped
        // back to Inactive/Suspended later without re-triggering setup).
        variability::messaging::complete_party_onboarding_request onboarding_req;
        onboarding_req.party_id = boost::uuids::to_string(party_id);
        auto onboarding_result =
            clientManager->process_authenticated_request(std::move(onboarding_req));
        if (!onboarding_result || !onboarding_result->success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Phase 4: complete_party_onboarding_request failed: "
                << (onboarding_result ? onboarding_result->message : "no server response")
                << " — party setup wizard may reappear on next login";
        }

        result.success = true;
        return result;
    });

    watcher->setFuture(future);
}

// ============================================================================
// PartyApplyAndSummaryPage
// ============================================================================

PartyApplyAndSummaryPage::PartyApplyAndSummaryPage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Party Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void PartyApplyAndSummaryPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* headerLabel = new QLabel(tr("Party setup complete"), this);
    headerLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    headerLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(headerLabel);

    layout->addSpacing(10);

    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    summaryLabel_->setTextFormat(Qt::RichText);
    summaryLabel_->setAlignment(Qt::AlignLeft | Qt::AlignTop);
    layout->addWidget(summaryLabel_);

    layout->addStretch();

    auto* logoutBox = new QGroupBox(tr("Action Required"), this);
    auto* logoutLayout = new QVBoxLayout(logoutBox);
    auto* logoutLabel =
        new QLabel(tr("<b>Log out and log back in to start using this party.</b><br><br>"
                      "After logging back in, select this party from the party selector "
                      "to access its portfolios, trading books, counterparties, and reports."),
                   this);
    logoutLabel->setWordWrap(true);
    logoutLabel->setTextFormat(Qt::RichText);
    logoutLayout->addWidget(logoutLabel);
    layout->addWidget(logoutBox);
}

void PartyApplyAndSummaryPage::initializePage() {
    const QString summary = tr("<p>This party is now active and ready for use.</p>");
    summaryLabel_->setText(summary);

    emit wizard_->provisioningCompleted();
}

} // namespace ores::qt
