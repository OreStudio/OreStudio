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
#include "ores.qt/QaValidationRunnerWidget.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.orgmode/indexing/resolver.hpp"
#include "ores.orgmode/parser/parser.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt.headless/EnvironmentMetadata.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt.headless/OrgDocRenderer.hpp"
#include "ores.qt/OrgDocViewerWindow.hpp"
#include "ores.qt.headless/RepoFileFinder.hpp"
#include "ores.qt.headless/TestScenarioResultsWriter.hpp"
#include <QColor>
#include <QDialogButtonBox>
#include <QDir>
#include <QFileDialog>
#include <QFileInfo>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QIcon>
#include <QLabel>
#include <QListWidget>
#include <QMdiArea>
#include <QRegularExpression>
#include <QPlainTextEdit>
#include <QProcess>
#include <QPushButton>
#include <QTabWidget>
#include <QTextBrowser>
#include <QToolBar>
#include <QUrl>
#include <QVBoxLayout>
#include <algorithm>
#include <memory>

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.qa_validation_runner_widget");
    return instance;
}

/**
 * @brief Find a top-level heading by exact title, or nullptr.
 */
const orgmode::domain::heading*
find_heading(const orgmode::domain::document& doc, const std::string& title) {
    for (const auto& h : doc.headings) {
        if (h.title == title)
            return &h;
    }
    return nullptr;
}

/**
 * @brief Look up a Field/Value table row by field name, across all of
 * @p heading's own tables (not its children's).
 */
QString find_field_value(const orgmode::domain::heading& heading, const std::string& field) {
    for (const auto& table : heading.tables) {
        for (const auto& row : table.rows) {
            if (row.size() >= 2 && row[0] == field)
                return QString::fromStdString(row[1]).trimmed();
        }
    }
    return {};
}

/**
 * @brief Extract the id from a raw =[[id:UUID][text]]= (or bare
 * =[[id:UUID]]=) cell value, or empty if it doesn't contain one.
 */
QString extract_link_id(const QString& cellValue) {
    static const QRegularExpression re(R"(\[\[id:([^\]]+)\])");
    const auto match = re.match(cellValue);
    return match.hasMatch() ? match.captured(1) : QString();
}

/**
 * @brief Find the =Verifies task=/=Parent story= link by field name
 * rather than table-row position — a scenario doc's Scenario Info
 * table happens to always list task before story today, but nothing
 * enforces that order, and a future third link would silently shift
 * =info->links= positionally.
 */
QString find_link_field_id(const orgmode::domain::heading& heading, const std::string& field) {
    return extract_link_id(find_field_value(heading, field));
}

step_status parse_step_status(const QString& text) {
    if (text.compare("PASS", Qt::CaseInsensitive) == 0)
        return step_status::pass;
    if (text.compare("FAIL", Qt::CaseInsensitive) == 0)
        return step_status::fail;
    return step_status::pending;
}

/**
 * @brief Build a qa_step from a step heading, pre-populating status/
 * notes from its own =*** Result= child if this step has already been
 * run before (so re-opening a scenario doesn't lose prior progress).
 */
qa_step make_step(const orgmode::domain::heading& step_heading, const QString& client) {
    qa_step step;
    step.title = QString::fromStdString(step_heading.title);
    step.bodyLines = step_heading.body_lines;
    step.client = client;
    for (const auto& child : step_heading.children) {
        if (child.title != "Result")
            continue;
        step.status = parse_step_status(find_field_value(child, "Status"));
        step.notes = find_field_value(child, "Notes");
        break;
    }
    return step;
}

QIcon status_icon(step_status status) {
    switch (status) {
    case step_status::pass:
        return IconUtils::createRecoloredIcon(Icon::CheckmarkCircleFilled, QColor(76, 175, 80));
    case step_status::fail:
        return IconUtils::createRecoloredIcon(Icon::DismissCircleFilled, QColor(229, 57, 53));
    case step_status::pending:
    default:
        return IconUtils::createRecoloredIcon(Icon::Clock, QColor(255, 193, 7));
    }
}

QString status_text(step_status status) {
    switch (status) {
    case step_status::pass:
        return QObject::tr("PASS");
    case step_status::fail:
        return QObject::tr("FAIL");
    case step_status::pending:
    default:
        return QObject::tr("PENDING");
    }
}

}

QaValidationRunnerWidget::QaValidationRunnerWidget(QWidget* parent) : QWidget(parent) {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    toolBar_ = new QToolBar(this);
    openAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::FolderOpen, IconUtils::DefaultIconColor), tr("Open"));
    openAction_->setToolTip(tr("Open a test_scenario doc"));
    saveAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor), tr("Save"));
    saveAction_->setToolTip(tr("Write step results back to the scenario doc"));
    saveAction_->setEnabled(false);
    layout->addWidget(toolBar_);

    tabWidget_ = new QTabWidget(this);
    layout->addWidget(tabWidget_, /*stretch=*/1);

    stepsTab_ = new QWidget(tabWidget_);
    auto* stepsTabLayout = new QVBoxLayout(stepsTab_);

    auto* infoBox = new QGroupBox(tr("Scenario Info"), stepsTab_);
    auto* infoLayout = new QVBoxLayout(infoBox);
    titleLabel_ = new QLabel(tr("No scenario loaded."), infoBox);
    targetDialogLabel_ = new QLabel(infoBox);
    overallStatusLabel_ = new QLabel(infoBox);
    infoLayout->addWidget(titleLabel_);
    infoLayout->addWidget(targetDialogLabel_);
    infoLayout->addWidget(overallStatusLabel_);
    stepsTabLayout->addWidget(infoBox);

    auto* stepsBox = new QGroupBox(tr("Steps"), stepsTab_);
    auto* stepsLayout = new QVBoxLayout(stepsBox);
    stepsList_ = new QListWidget(stepsBox);
    stepsLayout->addWidget(stepsList_);
    stepsTabLayout->addWidget(stepsBox, /*stretch=*/1);

    tabWidget_->addTab(stepsTab_, tr("Steps"));

    // Story/Task tabs: the driving docs, rendered read-only via
    // OrgDocRenderer, so the tester doesn't have to leave the app to
    // recover what the scenario is actually verifying. Separate tabs
    // (not stacked panes) since both docs are typically long enough
    // that stacking them left little room for either.
    auto make_context_tab = [this](const QString& tabTitle) {
        auto* tab = new QWidget(tabWidget_);
        auto* tabLayout = new QVBoxLayout(tab);
        auto* browser = new QTextBrowser(tab);
        browser->setOpenLinks(false);
        connect(browser, &QTextBrowser::anchorClicked, this,
            [this](const QUrl& url) { followContextLink(url); });
        tabLayout->addWidget(browser);
        tabWidget_->addTab(tab, tabTitle);
        return browser;
    };
    storyBrowser_ = make_context_tab(tr("Story"));
    taskBrowser_ = make_context_tab(tr("Task"));

    connect(openAction_, &QAction::triggered, this, &QaValidationRunnerWidget::promptAndLoadScenario);
    connect(saveAction_, &QAction::triggered, this, [this]() { save(); });
    connect(stepsList_, &QListWidget::itemDoubleClicked, this,
        &QaValidationRunnerWidget::openStepDetail);
}

void QaValidationRunnerWidget::promptAndLoadScenario() {
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Open test_scenario doc"), QString(), tr("Org files (*.org)"));
    if (!path.isEmpty())
        loadScenario(path);
}

bool QaValidationRunnerWidget::loadScenario(const QString& path) {
    orgmode::domain::document doc;
    try {
        doc = orgmode::parser::parse_file(path.toStdString());
    } catch (const std::exception& e) {
        emit errorOccurred(tr("Could not parse '%1': %2").arg(path, QString::fromStdString(e.what())));
        return false;
    }

    const auto* info = find_heading(doc, "Scenario Info");
    const auto* steps = find_heading(doc, "Steps");
    if (info == nullptr || steps == nullptr) {
        emit errorOccurred(tr("'%1' is missing a Scenario Info or Steps section — "
                              "is it really a test_scenario doc?")
                               .arg(path));
        return false;
    }

    scenarioPath_ = path;
    taskId_ = find_link_field_id(*info, "Verifies task");
    storyId_ = find_link_field_id(*info, "Parent story");

    titleLabel_->setText(QString::fromStdString(doc.find_keyword("title").value_or(path.toStdString())));
    const QString targetDialog = QString::fromStdString(doc.find_keyword("target_dialog").value_or(""));
    targetDialogLabel_->setText(
        targetDialog.isEmpty() ? tr("Target dialog: (none specified)")
                               : tr("Target dialog: %1").arg(targetDialog));

    // A step is a heading directly under * Steps. A multi-client
    // scenario nests steps one level deeper instead, under a
    // per-client sub-heading (see the test_scenario template) — signalled
    // by a non-empty Clients field in Scenario Info, not by structure
    // (a step's own *** Result child, once it's been run, would
    // otherwise be indistinguishable from a per-client grouping).
    const bool multi_client = !find_field_value(*info, "Clients").isEmpty();
    ++loadGeneration_;
    steps_.clear();
    for (const auto& child : steps->children) {
        if (multi_client) {
            const QString client = QString::fromStdString(child.title);
            for (const auto& grandchild : child.children)
                steps_.push_back(make_step(grandchild, client));
        } else {
            steps_.push_back(make_step(child, {}));
        }
    }
    rebuildStepList();
    updateOverallStatusLabel();
    loadContextTab(taskId_, storyId_);

    saveAction_->setEnabled(!steps_.empty());
    emit statusMessage(tr("Loaded %1").arg(path));
    return true;
}

void QaValidationRunnerWidget::rebuildStepList() {
    stepsList_->clear();
    for (const auto& step : steps_) {
        const QString label =
            step.client.isEmpty() ? step.title : QStringLiteral("[%1] %2").arg(step.client, step.title);
        auto* item = new QListWidgetItem(status_icon(step.status), label, stepsList_);
        item->setToolTip(label);
    }
}

void QaValidationRunnerWidget::updateStepItem(int index) {
    if (index < 0 || index >= stepsList_->count())
        return;
    auto* item = stepsList_->item(index);
    item->setIcon(status_icon(steps_[static_cast<std::size_t>(index)].status));
}

void QaValidationRunnerWidget::updateOverallStatusLabel() {
    if (steps_.empty()) {
        overallStatusLabel_->setText(tr("Status: (no steps)"));
        return;
    }
    const bool any_fail = std::any_of(steps_.begin(), steps_.end(),
        [](const qa_step& s) { return s.status == step_status::fail; });
    const bool all_pass = std::all_of(steps_.begin(), steps_.end(),
        [](const qa_step& s) { return s.status == step_status::pass; });

    QString text;
    QString color;
    if (any_fail) {
        text = tr("Status: FAILED");
        color = "#e53935";
    } else if (all_pass) {
        text = tr("Status: PASSED");
        color = "#4caf50";
    } else {
        text = tr("Status: PENDING");
        color = "#ffc107";
    }
    overallStatusLabel_->setText(text);
    overallStatusLabel_->setStyleSheet(QStringLiteral("font-weight: bold; color: %1;").arg(color));
}

void QaValidationRunnerWidget::openStepDetail(QListWidgetItem* item) {
    const int index = stepsList_->row(item);
    if (index < 0 || index >= static_cast<int>(steps_.size()))
        return;
    const auto& step = steps_[static_cast<std::size_t>(index)];
    const int generation = loadGeneration_;

    if (!mdiArea_) {
        emit errorOccurred(tr("No MDI area available to open the step in."));
        return;
    }

    // A plain content widget hosted in a DetachableMdiSubWindow — the
    // same convention every detail dialog in this app follows (see
    // AccountController etc.) — not a top-level QDialog/QWidget: those
    // get a window-manager "utility/always-above" hint (from QDialog's
    // default window type, or the WM's own transient-for handling)
    // regardless of modality, which pinned it above the main window
    // and even unrelated applications. An MDI subwindow has none of
    // that — it's just another window inside the app's own MDI area,
    // movable and non-blocking, letting the tester tick off several
    // steps side by side while running through a test.
    auto* content = new QWidget();
    auto* layout = new QVBoxLayout(content);

    auto* bodyBrowser = new QTextBrowser(content);
    const QString bodyHtml = render_body_lines_to_html(step.bodyLines);
    bodyBrowser->setHtml(bodyHtml.isEmpty() ? tr("<p>(no further instructions)</p>") : bodyHtml);
    layout->addWidget(bodyBrowser);

    auto* notesLabel = new QLabel(tr("Notes"), content);
    layout->addWidget(notesLabel);
    auto* notesEdit = new QPlainTextEdit(step.notes, content);
    layout->addWidget(notesEdit, /*stretch=*/1);

    auto* box = new QDialogButtonBox(QDialogButtonBox::Close, content);
    if (auto* closeButton = box->button(QDialogButtonBox::Close)) {
        closeButton->setIcon(IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
        closeButton->setText(tr("Close"));
    }
    auto* passButton = box->addButton(tr("Pass"), QDialogButtonBox::ActionRole);
    passButton->setIcon(status_icon(step_status::pass));
    auto* failButton = box->addButton(tr("Fail"), QDialogButtonBox::ActionRole);
    failButton->setIcon(status_icon(step_status::fail));
    auto* pendingButton = box->addButton(tr("Pending"), QDialogButtonBox::ActionRole);
    pendingButton->setIcon(status_icon(step_status::pending));
    layout->addWidget(box);

    auto* subWindow = new DetachableMdiSubWindow();
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->setWidget(content);
    subWindow->setWindowTitle(tr("Step: %1").arg(step.title));
    connect(box, &QDialogButtonBox::rejected, subWindow, &QMdiSubWindow::close);

    // index is captured by value and re-validated at click time, not a
    // reference into steps_: the tester could load a different scenario
    // (reallocating/shrinking steps_) while this subwindow is still open.
    // generation guards against the more insidious case — a *same-size*
    // reload where index would still be in bounds but now refers to an
    // entirely different scenario's step.
    auto apply = [this, index, generation, notesEdit, subWindow](step_status status) {
        if (generation == loadGeneration_ && index >= 0 &&
            index < static_cast<int>(steps_.size())) {
            auto& s = steps_[static_cast<std::size_t>(index)];
            s.notes = notesEdit->toPlainText();
            s.status = status;
            updateStepItem(index);
            updateOverallStatusLabel();
        }
        subWindow->close();
    };
    connect(passButton, &QPushButton::clicked, subWindow, [apply]() { apply(step_status::pass); });
    connect(failButton, &QPushButton::clicked, subWindow, [apply]() { apply(step_status::fail); });
    connect(pendingButton, &QPushButton::clicked, subWindow,
        [apply]() { apply(step_status::pending); });

    // Notes typed but not confirmed via a status button are still worth
    // keeping — persist on every edit rather than tying it to a
    // close/destroyed signal, which would risk running after the
    // widget tree (including notesEdit itself) has started tearing
    // down.
    connect(notesEdit, &QPlainTextEdit::textChanged, this, [this, index, generation, notesEdit]() {
        if (generation == loadGeneration_ && index >= 0 && index < static_cast<int>(steps_.size()))
            steps_[static_cast<std::size_t>(index)].notes = notesEdit->toPlainText();
    });

    mdiArea_->addSubWindow(subWindow);
    subWindow->resize(480, 360);
    subWindow->show();
}

bool QaValidationRunnerWidget::save() {
    if (scenarioPath_.isEmpty()) {
        emit errorOccurred(tr("No scenario loaded — nothing to save."));
        return false;
    }

    scenario_result result;
    const bool any_fail = std::any_of(steps_.begin(), steps_.end(),
        [](const qa_step& s) { return s.status == step_status::fail; });
    const bool all_pass = std::all_of(steps_.begin(), steps_.end(),
        [](const qa_step& s) { return s.status == step_status::pass; });
    result.status = any_fail ? "FAILED" : (all_pass ? "PASSED" : "PENDING");

    for (const auto& step : steps_)
        result.steps.push_back({step.title, status_text(step.status), step.notes, step.client});

    const auto environment = capture_environment_metadata(scenarioPath_);
    if (!write_scenario_results(scenarioPath_, result, environment)) {
        emit errorOccurred(tr("Failed to write results to '%1'.").arg(scenarioPath_));
        return false;
    }

    stampJournal(result.status);
    emit statusMessage(tr("Saved results to %1").arg(scenarioPath_));
    emit saved(scenarioPath_);
    return true;
}

void QaValidationRunnerWidget::stampJournal(const QString& status) {
    // Best-effort: a missing compass.sh, a task/story id we couldn't
    // extract, or a non-zero exit code shouldn't block the save that
    // already succeeded — just skip the journal stamp silently rather
    // than surface a second, confusing error after "Saved results".
    if (taskId_.isEmpty() || storyId_.isEmpty())
        return;
    const QString compass_sh = find_repo_file(scenarioPath_, "compass.sh");
    if (compass_sh.isEmpty())
        return;

    QProcess process;
    process.setWorkingDirectory(QFileInfo(compass_sh).absolutePath());
    process.start(compass_sh,
        {"journal", "update", "--story", storyId_, "--task", taskId_, "--branch",
            capture_environment_metadata(scenarioPath_).branch, "--state", status});
    process.waitForFinished(5000);
}

std::unique_ptr<orgmode::indexing::resolver> QaValidationRunnerWidget::openResolver() const {
    const QString db_path = find_repo_file(scenarioPath_, ".org-roam.db");
    if (db_path.isEmpty())
        return nullptr;
    try {
        return std::make_unique<orgmode::indexing::resolver>(db_path.toStdString());
    } catch (const std::exception&) {
        return nullptr;
    }
}

void QaValidationRunnerWidget::renderIdInto(orgmode::indexing::resolver* resolver,
                                            const QString& id,
                                            QTextBrowser* browser) {
    if (id.isEmpty()) {
        browser->setPlainText(tr("(No link found.)"));
        return;
    }
    if (!resolver) {
        browser->setPlainText(
            tr("(Could not resolve — is the repo's org-roam index built? Run 'compass index'.)"));
        return;
    }
    const auto target = resolver->resolve(id.toStdString());
    if (!target) {
        browser->setPlainText(
            tr("(Dangling link: id:%1 not found in the org-roam index.)").arg(id));
        return;
    }
    BOOST_LOG_SEV(lg(), debug) << "Resolved id:" << id.toStdString() << " -> " << target->path
                               << " (" << target->title << ")";
    try {
        const auto doc = orgmode::parser::parse_file(target->path);
        browser->setHtml(render_org_doc_to_html(doc));
    } catch (const std::exception& e) {
        browser->setPlainText(tr("(Could not parse '%1': %2)")
                                   .arg(QString::fromStdString(target->path),
                                        QString::fromStdString(e.what())));
    }
}

void QaValidationRunnerWidget::loadContextTab(const QString& taskId, const QString& storyId) {
    BOOST_LOG_SEV(lg(), debug) << "Loading context tab: taskId=" << taskId.toStdString()
                               << " storyId=" << storyId.toStdString();
    const auto resolver = openResolver();
    renderIdInto(resolver.get(), storyId, storyBrowser_);
    renderIdInto(resolver.get(), taskId, taskBrowser_);
}

void QaValidationRunnerWidget::followContextLink(const QUrl& url) {
    const QString target = url.toString();
    if (!target.startsWith("id:"))
        return;

    if (!mdiArea_) {
        emit errorOccurred(tr("No MDI area available to open the link in."));
        return;
    }

    // Opens a separate viewer (its own MDI subwindow, same as every
    // other detail window in the app — see openStepDetail()'s comment)
    // rather than navigating the Story/Task tab itself in place — the
    // tester doesn't lose what they were originally looking at, and
    // the new window's own Back/Forward lets them follow a whole trail
    // of further links.
    auto* viewer = new OrgDocViewerWindow(scenarioPath_);
    viewer->navigateToId(target.mid(3));

    auto* subWindow = new DetachableMdiSubWindow();
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->setWidget(viewer);
    connect(viewer, &OrgDocViewerWindow::windowTitleChanged, subWindow,
        &QWidget::setWindowTitle);
    mdiArea_->addSubWindow(subWindow);
    subWindow->resize(600, 500);
    subWindow->show();
}

}
